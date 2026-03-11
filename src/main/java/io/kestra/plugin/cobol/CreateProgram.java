package io.kestra.plugin.cobol;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.UUID;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.slf4j.Logger;

import com.ibm.as400.access.*;

import io.kestra.core.models.annotations.Example;
import io.kestra.core.models.annotations.Plugin;
import io.kestra.core.models.property.Property;
import io.kestra.core.models.tasks.RunnableTask;
import io.kestra.core.runners.RunContext;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import lombok.*;
import lombok.experimental.SuperBuilder;

@SuperBuilder
@ToString
@EqualsAndHashCode
@Getter
@NoArgsConstructor
@Schema(
    title = "Create (compile) a COBOL program on IBM i.",
    description = "Uploads COBOL source to an IFS temporary file, then compiles using CRTCBLPGM via JTOpen CommandCall. " +
        "Source can be provided inline or via a URI to a COBOL source file."
)
@Plugin(
    examples = {
        @Example(
            title = "Compile a COBOL program from a downloaded source",
            full = true,
            code = """
                id: create_cobol
                namespace: company.team

                tasks:
                  - id: download_source
                    type: io.kestra.plugin.core.http.Download
                    uri: https://repo.mybank.com/cobol/CALCINT.cbl

                  - id: compile
                    type: io.kestra.plugin.cobol.CreateProgram
                    host: "{{ secret('IBM_HOST') }}"
                    user: "{{ secret('IBM_USER') }}"
                    password: "{{ secret('IBM_PASSWORD') }}"
                    library: FINLIB
                    program: CALCINT
                    sourceUri: "{{ outputs.download_source.uri }}"
                """
        ),
        @Example(
            title = "Compile a COBOL program from inline source",
            full = true,
            code = """
                id: create_cobol_inline
                namespace: company.team

                tasks:
                  - id: compile
                    type: io.kestra.plugin.cobol.CreateProgram
                    host: "{{ secret('IBM_HOST') }}"
                    user: "{{ secret('IBM_USER') }}"
                    password: "{{ secret('IBM_PASSWORD') }}"
                    library: DEVLIB
                    program: HELLO
                    sourceInline: |
                           IDENTIFICATION DIVISION.
                           PROGRAM-ID. HELLO.
                           PROCEDURE DIVISION.
                               DISPLAY 'HELLO FROM KESTRA'.
                               STOP RUN.
                """
        )
    }
)
public class CreateProgram extends AbstractAs400Connection implements RunnableTask<CreateProgram.Output> {
    private static final Pattern COMPILE_OPTIONS_PATTERN = Pattern.compile("^[A-Za-z][A-Za-z0-9_]*\\([^()]*\\)(\\s+[A-Za-z][A-Za-z0-9_]*\\([^()]*\\))*$");

    @Schema(
        title = "IBM i library.",
        description = "The target library where the compiled program will be created (e.g., `FINLIB`)."
    )
    @NotNull
    private Property<String> library;

    @Schema(
        title = "Program name.",
        description = "The name for the compiled program object (e.g., `CALCINT`)."
    )
    @NotNull
    private Property<String> program;

    @Schema(
        title = "Inline COBOL source code.",
        description = "The COBOL source code provided directly as a string. Either `sourceInline` or `sourceUri` must be provided, but not both."
    )
    private Property<String> sourceInline;

    @Schema(
        title = "URI to a COBOL source file.",
        description = "A Kestra internal storage URI pointing to a COBOL source file. " +
            "Use a preceding download task for remote sources. " +
            "Either `sourceUri` or `sourceInline` must be provided, but not both."
    )
    private Property<String> sourceUri;

    @Schema(
        title = "Additional compile options.",
        description = "Extra options to pass to CRTCBLPGM (e.g., `DBGVIEW(*ALL)`)."
    )
    private Property<String> compileOptions;

    @Override
    public Output run(RunContext runContext) throws Exception {
        Logger logger = runContext.logger();

        var rLibrary = requireSimpleObjectName(runContext.render(this.library).as(String.class).orElseThrow(), "library");
        var rProgram = requireSimpleObjectName(runContext.render(this.program).as(String.class).orElseThrow(), "program");
        var rInline = runContext.render(this.sourceInline).as(String.class).orElse(null);
        var rUri = runContext.render(this.sourceUri).as(String.class).orElse(null);

        // Validate exactly one source is provided
        if (rInline == null && rUri == null) {
            throw new IllegalArgumentException("Either 'sourceInline' or 'sourceUri' must be provided.");
        }
        if (rInline != null && rUri != null) {
            throw new IllegalArgumentException("Only one of 'sourceInline' or 'sourceUri' may be provided, not both.");
        }

        // Resolve source content
        var sourceContent = rInline != null ? rInline : readSourceFromUri(runContext, rUri);

        var programPath = "/QSYS.LIB/" + rLibrary + ".LIB/" + rProgram + ".PGM";
        logger.info("Creating program: {}", programPath);

        var system = this.connect(runContext);
        try {
            // Upload source to IFS temporary file
            var ifsPath = "/tmp/kestra_" + rProgram + "_" + UUID.randomUUID() + ".cbl";
            uploadSourceToIfs(system, ifsPath, sourceContent);
            logger.debug("Source uploaded to IFS: {}", ifsPath);

            // Build CRTCBLPGM command
            var crtCmd = new StringBuilder();
            crtCmd.append("CRTCBLPGM PGM(")
                .append(rLibrary).append("/").append(rProgram)
                .append(") SRCSTMF('").append(ifsPath).append("')");

            var rOptions = sanitizeCompileOptions(runContext.render(this.compileOptions).as(String.class).orElse(null));
            if (rOptions != null && !rOptions.isBlank()) {
                crtCmd.append(" ").append(rOptions);
            }

            var command = crtCmd.toString();
            logger.info("Compiling: {}", command);

            var cmd = new CommandCall(system);
            var success = cmd.run(command);

            // Extract compile messages
            var messages = extractMessages(cmd.getMessageList());

            // Clean up IFS temp file (best-effort)
            try {
                var tempFile = new IFSFile(system, ifsPath);
                tempFile.delete();
            } catch (Exception e) {
                logger.warn("Could not remove temporary IFS file {}: {}", ifsPath, e.getMessage());
            }

            if (!success) {
                var errorDetail = messages.stream()
                    .map(m -> m.getId() + ": " + m.getText())
                    .collect(Collectors.joining("; "));
                logger.error("CRTCBLPGM failed: {}", errorDetail);
                throw new IllegalStateException("CRTCBLPGM failed: " + errorDetail);
            }

            logger.info("Program {} created successfully", programPath);

            return Output.builder()
                .programPath(programPath)
                .compileMessages(messages)
                .build();
        } finally {
            system.disconnectAllServices();
        }
    }

    private String readSourceFromUri(RunContext runContext, String uri) throws Exception {
        URI sourceUri = URI.create(uri);
        try (
            InputStream is = runContext.storage().getFile(sourceUri);
            BufferedReader reader = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))
        ) {
            return reader.lines().collect(Collectors.joining("\n"));
        }
    }

    private void uploadSourceToIfs(AS400 system, String ifsPath, String content) throws Exception {
        var ifsFile = new IFSFile(system, ifsPath);
        try (var os = new IFSFileOutputStream(ifsFile)) {
            os.write(content.getBytes(StandardCharsets.UTF_8));
            os.flush();
        }
    }

    private String sanitizeCompileOptions(String compileOptions) {
        if (compileOptions == null) {
            return null;
        }

        var normalized = compileOptions.trim();
        if (normalized.isEmpty()) {
            return null;
        }

        if (!COMPILE_OPTIONS_PATTERN.matcher(normalized).matches()) {
            throw new IllegalArgumentException("Property 'compileOptions' must be a space-separated list of CL options in NAME(VALUE) format.");
        }

        return normalized;
    }

    @Builder
    @Getter
    public static class Output implements io.kestra.core.models.tasks.Output {
        @Schema(title = "IFS path of the created program object.")
        private final String programPath;

        @Schema(title = "Compile messages returned by the IBM i system.")
        private final List<MessageOutput> compileMessages;
    }

}
