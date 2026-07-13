package io.kestra.plugin.cobol;

import java.util.Collections;
import java.util.List;
import java.util.StringJoiner;
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
import io.kestra.core.models.annotations.PluginProperty;

@SuperBuilder
@ToString
@EqualsAndHashCode
@Getter
@NoArgsConstructor
@Schema(
    title = "Submit an IBM i COBOL job asynchronously",
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
    private static final Pattern JOB_IDENTITY_PATTERN = Pattern.compile("\\b(\\d{6})/([A-Za-z0-9_$#@]+)/([A-Za-z0-9_$#@]+)\\b");

    @Schema(
        title = "IBM i library",
        description = "The library containing the program to submit (e.g., `BATCHLIB`)."
    )
    @NotNull
    @PluginProperty(group = "main")
    private Property<String> library;

    @Schema(
        title = "Program name",
        description = "The name of the program to submit (e.g., `EODPROC`)."
    )
    @NotNull
    @PluginProperty(group = "main")
    private Property<String> program;

    @Schema(
        title = "Program parameters",
        description = "List of string parameters to pass to the program via PARM()."
    )
    @Builder.Default
    @PluginProperty(group = "advanced")
    private Property<List<String>> parameters = Property.ofValue(Collections.emptyList());

    @Schema(
        title = "Job name",
        description = "Optional name for the submitted job. If not specified, the system assigns one."
    )
    @PluginProperty(group = "advanced")
    private Property<String> jobName;

    @Schema(
        title = "Job queue",
        description = "The job queue to submit the job to (e.g., `QBATCH`). If not specified, the default job queue is used."
    )
    @PluginProperty(group = "advanced")
    private Property<String> jobQueue;

    @Schema(
        title = "User profile",
        description = "The user profile under which the job runs. Defaults to the connection user."
    )
    @PluginProperty(group = "advanced")
    private Property<String> userProfile;

    @Override
    public Output run(RunContext runContext) throws Exception {
        Logger logger = runContext.logger();

        var rLibrary = requireSimpleObjectName(runContext.render(this.library).as(String.class).orElseThrow(), "library");
        var rProgram = requireSimpleObjectName(runContext.render(this.program).as(String.class).orElseThrow(), "program");
        var rParams = runContext.render(this.parameters).asList(String.class);

        // Build the CALL PGM command with PARM if needed
        var callCmd = new StringBuilder();
        callCmd.append("CALL PGM(").append(rLibrary).append("/").append(rProgram).append(")");
        if (!rParams.isEmpty()) {
            callCmd.append(" PARM(").append(formatParameters(rParams)).append(")");
        }

        // Build the SBMJOB command
        var sbmCmd = new StringBuilder();
        sbmCmd.append("SBMJOB CMD(").append(callCmd).append(")");

        var rJobName = runContext.render(this.jobName).as(String.class).orElse(null);
        if (rJobName != null) {
            sbmCmd.append(" JOB(").append(requireSimpleObjectName(rJobName, "jobName")).append(")");
        }

        var rJobQueue = runContext.render(this.jobQueue).as(String.class).orElse(null);
        if (rJobQueue != null) {
            sbmCmd.append(" JOBQ(").append(requireQualifiedObjectName(rJobQueue, "jobQueue")).append(")");
        }

        var rUserProfile = runContext.render(this.userProfile).as(String.class).orElse(null);
        if (rUserProfile != null) {
            sbmCmd.append(" USER(").append(requireSimpleObjectName(rUserProfile, "userProfile")).append(")");
        }

        var command = sbmCmd.toString();
        logger.info("Submitting job: {}", command);

        AS400 system = this.connect(runContext);
        try {
            CommandCall cmd = new CommandCall(system);
            boolean success = cmd.run(command);

            // Extract messages
            List<MessageOutput> messages = extractMessages(cmd.getMessageList());

            var submittedJob = extractSubmittedJob(messages);
            var resJobName = submittedJob != null ? submittedJob.name() : rJobName;
            var resJobNumber = submittedJob != null ? submittedJob.number() : null;
            var resJobUser = submittedJob != null ? submittedJob.user() : rUserProfile;

            if (!success) {
                var errorDetail = messages.stream()
                    .map(m -> m.getId() + ": " + m.getText())
                    .collect(Collectors.joining("; "));
                logger.error("SBMJOB failed: {}", errorDetail);
                throw new IllegalStateException("SBMJOB failed: " + errorDetail);
            }

            if (submittedJob != null) {
                logger.info("Job submitted successfully: {}/{}/{}", resJobNumber, resJobUser, resJobName);
            } else {
                logger.info("Job submitted successfully, but submitted job identity was not returned by IBM i messages");
            }

            return Output.builder()
                .messages(messages)
                .jobName(resJobName)
                .jobNumber(resJobNumber)
                .jobUser(resJobUser)
                .build();
        } finally {
            system.disconnectAllServices();
        }
    }

    private SubmittedJob extractSubmittedJob(List<MessageOutput> messages) {
        var submittedFromKnownMessage = findSubmittedJob(messages, true);
        if (submittedFromKnownMessage != null) {
            return submittedFromKnownMessage;
        }

        return findSubmittedJob(messages, false);
    }

    private SubmittedJob findSubmittedJob(List<MessageOutput> messages, boolean onlyKnownSubmittedMessageId) {
        for (var message : messages) {
            if (message.getText() == null) {
                continue;
            }

            if (onlyKnownSubmittedMessageId && !"CPC1221".equals(message.getId())) {
                continue;
            }

            var matcher = JOB_IDENTITY_PATTERN.matcher(message.getText());
            if (matcher.find()) {
                return new SubmittedJob(matcher.group(1), matcher.group(2), matcher.group(3));
            }
        }

        return null;
    }

    private String formatParameters(List<String> rParams) {
        var parameterJoiner = new StringJoiner(" ");
        for (var parameter : rParams) {
            parameterJoiner.add("'" + escapeClStringLiteral(parameter) + "'");
        }
        return parameterJoiner.toString();
    }

    private record SubmittedJob(String number, String user, String name) {
    }

    @Builder
    @Getter
    public static class Output implements io.kestra.core.models.tasks.Output {
        @Schema(title = "Messages returned by the IBM i system")
        private final List<MessageOutput> messages;

        @Schema(title = "Name of the submitted job")
        private final String jobName;

        @Schema(title = "Number of the submitted job")
        private final String jobNumber;

        @Schema(title = "User profile of the submitted job")
        private final String jobUser;
    }

}
