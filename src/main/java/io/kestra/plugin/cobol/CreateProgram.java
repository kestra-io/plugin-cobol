package io.kestra.plugin.cobol;

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
import org.slf4j.Logger;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

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
            title = "Compile a COBOL program from a remote source",
            full = true,
            code = """
                id: create_cobol
                namespace: company.team

                tasks:
                  - id: compile
                    type: io.kestra.plugin.cobol.CreateProgram
                    host: "{{ secret('IBM_HOST') }}"
                    user: "{{ secret('IBM_USER') }}"
                    password: "{{ secret('IBM_PASSWORD') }}"
                    library: FINLIB
                    program: CALCINT
                    sourceUri: https://repo.mybank.com/cobol/CALCINT.cbl
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
        description = "A URI pointing to a COBOL source file (Kestra internal storage URI or HTTP URL). " +
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

        String renderedLibrary = runContext.render(this.library).as(String.class).orElseThrow();
        String renderedProgram = runContext.render(this.program).as(String.class).orElseThrow();
        String renderedInline = runContext.render(this.sourceInline).as(String.class).orElse(null);
        String renderedUri = runContext.render(this.sourceUri).as(String.class).orElse(null);

        // Validate exactly one source is provided
        if (renderedInline == null && renderedUri == null) {
            throw new IllegalArgumentException("Either 'sourceInline' or 'sourceUri' must be provided.");
        }
        if (renderedInline != null && renderedUri != null) {
            throw new IllegalArgumentException("Only one of 'sourceInline' or 'sourceUri' may be provided, not both.");
        }

        // Resolve source content
        String sourceContent;
        if (renderedInline != null) {
            sourceContent = renderedInline;
        } else {
            sourceContent = readSourceFromUri(runContext, renderedUri);
        }

        String programPath = "/QSYS.LIB/" + renderedLibrary + ".LIB/" + renderedProgram + ".PGM";
        logger.info("Creating program: {}", programPath);

        AS400 system = this.connect(runContext);
        try {
            // Upload source to IFS temporary file
            String ifsPath = "/tmp/kestra_" + renderedProgram + ".cbl";
            uploadSourceToIfs(system, ifsPath, sourceContent);
            logger.debug("Source uploaded to IFS: {}", ifsPath);

            // Build CRTCBLPGM command
            StringBuilder crtCmd = new StringBuilder();
            crtCmd.append("CRTCBLPGM PGM(")
                .append(renderedLibrary).append("/").append(renderedProgram)
                .append(") SRCSTMF('").append(ifsPath).append("')");

            String renderedOptions = runContext.render(this.compileOptions).as(String.class).orElse(null);
            if (renderedOptions != null && !renderedOptions.isBlank()) {
                crtCmd.append(" ").append(renderedOptions);
            }

            String command = crtCmd.toString();
            logger.info("Compiling: {}", command);

            CommandCall cmd = new CommandCall(system);
            boolean success = cmd.run(command);

            // Extract compile messages
            List<MessageOutput> messages = extractMessages(cmd.getMessageList());

            // Clean up IFS temp file (best-effort)
            try {
                IFSFile tempFile = new IFSFile(system, ifsPath);
                tempFile.delete();
            } catch (Exception e) {
                logger.warn("Could not remove temporary IFS file {}: {}", ifsPath, e.getMessage());
            }

            if (!success) {
                String errorDetail = messages.stream()
                    .map(m -> m.getId() + ": " + m.getText())
                    .collect(Collectors.joining("; "));
                logger.error("CRTCBLPGM failed: {}", errorDetail);
                throw new Exception("CRTCBLPGM failed: " + errorDetail);
            }

            logger.info("Program {} created successfully", programPath);

            return Output.builder()
                .success(true)
                .programPath(programPath)
                .compileMessages(messages)
                .build();
        } finally {
            system.disconnectAllServices();
        }
    }

    private String readSourceFromUri(RunContext runContext, String uri) throws Exception {
        URI sourceUri = URI.create(uri);
        try (InputStream is = runContext.storage().getFile(sourceUri);
             BufferedReader reader = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))) {
            return reader.lines().collect(Collectors.joining("\n"));
        }
    }

    private void uploadSourceToIfs(AS400 system, String ifsPath, String content) throws Exception {
        IFSFile ifsFile = new IFSFile(system, ifsPath);
        try (OutputStream os = new IFSFileOutputStream(ifsFile)) {
            os.write(content.getBytes(StandardCharsets.UTF_8));
            os.flush();
        }
    }

    private List<MessageOutput> extractMessages(AS400Message[] messageList) {
        if (messageList == null || messageList.length == 0) {
            return Collections.emptyList();
        }

        List<MessageOutput> messages = new ArrayList<>(messageList.length);
        for (AS400Message msg : messageList) {
            messages.add(MessageOutput.builder()
                .id(msg.getID())
                .text(msg.getText())
                .severity(msg.getSeverity())
                .build());
        }
        return messages;
    }

    @Builder
    @Getter
    public static class Output implements io.kestra.core.models.tasks.Output {
        @Schema(title = "Whether the compilation was successful.")
        private final Boolean success;

        @Schema(title = "IFS path of the created program object.")
        private final String programPath;

        @Schema(title = "Compile messages returned by the IBM i system.")
        private final List<MessageOutput> compileMessages;
    }

    @Builder
    @Getter
    public static class MessageOutput {
        @Schema(title = "IBM i message ID (e.g., LNC0011).")
        private final String id;

        @Schema(title = "Message text.")
        private final String text;

        @Schema(title = "Message severity level.")
        private final Integer severity;
    }
}
