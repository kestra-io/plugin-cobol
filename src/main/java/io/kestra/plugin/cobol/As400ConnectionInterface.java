package io.kestra.plugin.cobol;

import io.kestra.core.models.annotations.PluginProperty;
import io.kestra.core.models.property.Property;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;

/**
 * Interface defining IBM i (AS/400) connection properties.
 * All tasks that connect to an IBM i system implement this interface.
 */
public interface As400ConnectionInterface {

    @Schema(
        title = "IBM i hostname.",
        description = "The hostname or IP address of the IBM i (AS/400) system."
    )
    @NotNull
    @PluginProperty(group = "connection")
    Property<String> getHost();

    @Schema(
        title = "IBM i user profile.",
        description = "The user profile to authenticate with on the IBM i system."
    )
    @NotNull
    @PluginProperty(group = "connection")
    Property<String> getUser();

    @Schema(
        title = "IBM i password.",
        description = "The password for the IBM i user profile."
    )
    @NotNull
    @PluginProperty(group = "connection")
    Property<String> getPassword();
}
