package io.kestra.plugin.cobol;

import com.ibm.as400.access.AS400;
import com.ibm.as400.access.AS400Message;
import io.kestra.core.models.property.Property;
import io.kestra.core.models.tasks.Task;
import io.kestra.core.runners.RunContext;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

/**
 * Base class for tasks that connect to an IBM i (AS/400) system.
 * Provides connection fields and a factory method for creating AS400 instances.
 */
@SuperBuilder
@ToString
@EqualsAndHashCode
@Getter
@NoArgsConstructor
public abstract class AbstractAs400Connection extends Task implements As400ConnectionInterface {

    @Schema(
        title = "IBM i hostname.",
        description = "The hostname or IP address of the IBM i (AS/400) system."
    )
    @NotNull
    private Property<String> host;

    @Schema(
        title = "IBM i user profile.",
        description = "The user profile to authenticate with on the IBM i system."
    )
    @NotNull
    private Property<String> user;

    @Schema(
        title = "IBM i password.",
        description = "The password for the IBM i user profile."
    )
    @NotNull
    private Property<String> password;

    /**
     * Creates an AS400 connection from the rendered task properties.
     * The caller is responsible for disconnecting the returned instance.
     */
    protected AS400 connect(RunContext runContext) throws Exception {
        String rHost = runContext.render(this.host).as(String.class).orElseThrow();
        String rUser = runContext.render(this.user).as(String.class).orElseThrow();
        String rPassword = runContext.render(this.password).as(String.class).orElseThrow();

        // Prevent GUI prompts in headless environments (mirrors plugin-jdbc-as400)
        AS400.setDefaultSignonHandler(new NonInteractiveSignonHandler());
        System.setProperty("com.ibm.as400.access.AS400.guiAvailable", "false");

        AS400 system = new AS400(rHost, rUser, rPassword);
        return system;
    }

    /**
     * Converts JTOpen AS400Message array to a list of MessageOutput.
     */
    protected List<MessageOutput> extractMessages(AS400Message[] messageList) {
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
}
