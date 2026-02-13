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
    title = "Submit an IBM i COBOL job asynchronously.",
    description = "Submits a job to an IBM i (AS/400) system using JTOpen CommandCall with SBMJOB. " +
        "The job is submitted and control returns immediately without waiting for completion."
)
@Plugin(
    examples = {
        @Example(
            title = "Submit a COBOL batch job",
            full = true,
            code = """
                id: submit_cobol
                namespace: company.team

                tasks:
                  - id: submit
                    type: io.kestra.plugin.cobol.SubmitJob
                    host: "{{ secret('IBM_HOST') }}"
                    user: "{{ secret('IBM_USER') }}"
                    password: "{{ secret('IBM_PASSWORD') }}"
                    library: BATCHLIB
                    program: EODPROC
                    jobQueue: QBATCH
                """
        )
    }
)
public class SubmitJob extends AbstractAs400Connection implements RunnableTask<SubmitJob.Output> {

    @Schema(
        title = "IBM i library.",
        description = "The library containing the program to submit (e.g., `BATCHLIB`)."
    )
    @NotNull
    private Property<String> library;

    @Schema(
        title = "Program name.",
        description = "The name of the program to submit (e.g., `EODPROC`)."
    )
    @NotNull
    private Property<String> program;

    @Schema(
        title = "Program parameters.",
        description = "List of string parameters to pass to the program via PARM()."
    )
    @Builder.Default
    private Property<List<String>> parameters = Property.ofValue(Collections.emptyList());

    @Schema(
        title = "Job name.",
        description = "Optional name for the submitted job. If not specified, the system assigns one."
    )
    private Property<String> jobName;

    @Schema(
        title = "Job queue.",
        description = "The job queue to submit the job to (e.g., `QBATCH`). If not specified, the default job queue is used."
    )
    private Property<String> jobQueue;

    @Schema(
        title = "User profile.",
        description = "The user profile under which the job runs. Defaults to the connection user."
    )
    private Property<String> userProfile;

    @Override
    public Output run(RunContext runContext) throws Exception {
        Logger logger = runContext.logger();

        String renderedLibrary = runContext.render(this.library).as(String.class).orElseThrow();
        String renderedProgram = runContext.render(this.program).as(String.class).orElseThrow();
        List<String> renderedParams = runContext.render(this.parameters).asList(String.class);

        // Build the CALL PGM command with PARM if needed
        StringBuilder callCmd = new StringBuilder();
        callCmd.append("CALL PGM(").append(renderedLibrary).append("/").append(renderedProgram).append(")");
        if (!renderedParams.isEmpty()) {
            String parmList = renderedParams.stream()
                .map(p -> "'" + p + "'")
                .collect(Collectors.joining(" "));
            callCmd.append(" PARM(").append(parmList).append(")");
        }

        // Build the SBMJOB command
        StringBuilder sbmCmd = new StringBuilder();
        sbmCmd.append("SBMJOB CMD(").append(callCmd).append(")");

        String renderedJobName = runContext.render(this.jobName).as(String.class).orElse(null);
        if (renderedJobName != null) {
            sbmCmd.append(" JOB(").append(renderedJobName).append(")");
        }

        String renderedJobQueue = runContext.render(this.jobQueue).as(String.class).orElse(null);
        if (renderedJobQueue != null) {
            sbmCmd.append(" JOBQ(").append(renderedJobQueue).append(")");
        }

        String renderedUserProfile = runContext.render(this.userProfile).as(String.class).orElse(null);
        if (renderedUserProfile != null) {
            sbmCmd.append(" USER(").append(renderedUserProfile).append(")");
        }

        String command = sbmCmd.toString();
        logger.info("Submitting job: {}", command);

        AS400 system = this.connect(runContext);
        try {
            CommandCall cmd = new CommandCall(system);
            boolean success = cmd.run(command);

            // Extract messages
            List<MessageOutput> messages = extractMessages(cmd.getMessageList());

            // Extract job info from the server job
            Job serverJob = cmd.getServerJob();
            String resJobName = serverJob != null ? serverJob.getName() : null;
            String resJobNumber = serverJob != null ? serverJob.getNumber() : null;
            String resJobUser = serverJob != null ? serverJob.getUser() : null;

            if (!success) {
                String errorDetail = messages.stream()
                    .map(m -> m.getId() + ": " + m.getText())
                    .collect(Collectors.joining("; "));
                logger.error("SBMJOB failed: {}", errorDetail);
                throw new Exception("SBMJOB failed: " + errorDetail);
            }

            logger.info("Job submitted successfully: {}/{}/{}", resJobNumber, resJobUser, resJobName);

            return Output.builder()
                .submitted(true)
                .messages(messages)
                .jobName(resJobName)
                .jobNumber(resJobNumber)
                .jobUser(resJobUser)
                .build();
        } finally {
            system.disconnectAllServices();
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
        @Schema(title = "Whether the job was submitted successfully.")
        private final Boolean submitted;

        @Schema(title = "Messages returned by the IBM i system.")
        private final List<MessageOutput> messages;

        @Schema(title = "Name of the submitted job.")
        private final String jobName;

        @Schema(title = "Number of the submitted job.")
        private final String jobNumber;

        @Schema(title = "User profile of the submitted job.")
        private final String jobUser;
    }

    @Builder
    @Getter
    public static class MessageOutput {
        @Schema(title = "IBM i message ID (e.g., CPC1221).")
        private final String id;

        @Schema(title = "Message text.")
        private final String text;

        @Schema(title = "Message severity level.")
        private final Integer severity;
    }
}
