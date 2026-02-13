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

import java.time.Duration;
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
    title = "Call an IBM i COBOL program synchronously.",
    description = "Executes an existing program on an IBM i (AS/400) system using JTOpen ProgramCall and waits for completion."
)
@Plugin(
    examples = {
        @Example(
            title = "Call a COBOL program",
            full = true,
            code = """
                id: call_cobol
                namespace: company.team

                tasks:
                  - id: call
                    type: io.kestra.plugin.cobol.CallJob
                    host: "{{ secret('IBM_HOST') }}"
                    user: "{{ secret('IBM_USER') }}"
                    password: "{{ secret('IBM_PASSWORD') }}"
                    library: FINLIB
                    program: CALCINT
                    parameters:
                      - "2026-01-31"
                """
        )
    }
)
public class CallJob extends AbstractAs400Connection implements RunnableTask<CallJob.Output> {

    @Schema(
        title = "IBM i library.",
        description = "The library containing the program to call (e.g., `FINLIB`)."
    )
    @NotNull
    private Property<String> library;

    @Schema(
        title = "Program name.",
        description = "The name of the program to call (e.g., `CALCINT`)."
    )
    @NotNull
    private Property<String> program;

    @Schema(
        title = "Program parameters.",
        description = "List of string parameters to pass to the program. Each string is converted to EBCDIC using the system CCSID."
    )
    @Builder.Default
    private Property<List<String>> parameters = Property.ofValue(Collections.emptyList());

    @Schema(
        title = "Program call timeout in seconds.",
        description = "Maximum time in seconds to wait for the program to complete. If not set, no timeout is applied."
    )
    private Property<Integer> programTimeout;

    @Override
    public Output run(RunContext runContext) throws Exception {
        Logger logger = runContext.logger();

        String renderedLibrary = runContext.render(this.library).as(String.class).orElseThrow();
        String renderedProgram = runContext.render(this.program).as(String.class).orElseThrow();
        List<String> renderedParams = runContext.render(this.parameters).asList(String.class);

        String programPath = "/QSYS.LIB/" + renderedLibrary + ".LIB/" + renderedProgram + ".PGM";
        logger.info("Calling program: {}", programPath);

        AS400 system = this.connect(runContext);
        try {
            ProgramCall pgm = new ProgramCall(system);

            // Build program parameters
            ProgramParameter[] pgmParams = buildParameters(renderedParams, system);
            pgm.setProgram(programPath, pgmParams);

            // Set timeout if specified
            Integer renderedTimeout = runContext.render(this.programTimeout).as(Integer.class).orElse(null);
            if (renderedTimeout != null) {
                pgm.setTimeOut(renderedTimeout);
                logger.debug("Timeout set to {} seconds", renderedTimeout);
            }

            // Execute
            long startTime = System.nanoTime();
            boolean success = pgm.run();
            Duration duration = Duration.ofNanos(System.nanoTime() - startTime);

            // Extract messages
            List<MessageOutput> messages = extractMessages(pgm.getMessageList());

            // Extract job info
            Job serverJob = pgm.getServerJob();
            String jobName = serverJob != null ? serverJob.getName() : null;
            String jobNumber = serverJob != null ? serverJob.getNumber() : null;
            String jobUser = serverJob != null ? serverJob.getUser() : null;

            if (!success) {
                String errorDetail = messages.stream()
                    .map(m -> m.getId() + ": " + m.getText())
                    .collect(Collectors.joining("; "));
                logger.error("Program {} failed: {}", programPath, errorDetail);
                throw new Exception("Program call failed: " + errorDetail);
            }

            logger.info("Program {} completed successfully in {}", programPath, duration);

            return Output.builder()
                .returnCode(0)
                .messages(messages)
                .jobName(jobName)
                .jobNumber(jobNumber)
                .jobUser(jobUser)
                .duration(duration)
                .build();
        } finally {
            system.disconnectAllServices();
        }
    }

    private ProgramParameter[] buildParameters(List<String> params, AS400 system) {
        if (params == null || params.isEmpty()) {
            return new ProgramParameter[0];
        }

        ProgramParameter[] pgmParams = new ProgramParameter[params.size()];
        for (int i = 0; i < params.size(); i++) {
            String value = params.get(i);
            AS400Text textConverter = new AS400Text(value.length(), system);
            pgmParams[i] = new ProgramParameter(textConverter.toBytes(value));
        }
        return pgmParams;
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
        @Schema(title = "Return code of the program call (0 = success).")
        private final Integer returnCode;

        @Schema(title = "Messages returned by the IBM i system.")
        private final List<MessageOutput> messages;

        @Schema(title = "Name of the server job that ran the program.")
        private final String jobName;

        @Schema(title = "Number of the server job.")
        private final String jobNumber;

        @Schema(title = "User profile of the server job.")
        private final String jobUser;

        @Schema(title = "Wall-clock duration of the program call.")
        private final Duration duration;
    }

    @Builder
    @Getter
    public static class MessageOutput {
        @Schema(title = "IBM i message ID (e.g., CPF9801).")
        private final String id;

        @Schema(title = "Message text.")
        private final String text;

        @Schema(title = "Message severity level.")
        private final Integer severity;
    }
}
