package io.kestra.plugin.cobol;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import io.kestra.core.models.annotations.PluginProperty;

/**
 * Structured representation of an IBM i system message.
 * Shared across all tasks that interact with the IBM i message queue.
 */
@Builder
@Getter
public class MessageOutput {
    @Schema(title = "IBM i message ID (e.g., CPF9801).")
    @PluginProperty(group = "advanced")
    private final String id;

    @Schema(title = "Message text.")
    @PluginProperty(group = "advanced")
    private final String text;

    @Schema(title = "Message severity level.")
    @PluginProperty(group = "advanced")
    private final Integer severity;
}
