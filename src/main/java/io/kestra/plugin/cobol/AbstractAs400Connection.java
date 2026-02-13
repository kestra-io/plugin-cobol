package io.kestra.plugin.cobol;

import com.ibm.as400.access.AS400;
import io.kestra.core.models.property.Property;
import io.kestra.core.models.tasks.Task;
import io.kestra.core.runners.RunContext;
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

    private Property<String> host;

    private Property<String> user;

    private Property<String> password;

    /**
     * Creates an AS400 connection from the rendered task properties.
     * The caller is responsible for disconnecting the returned instance.
     */
    protected AS400 connect(RunContext runContext) throws Exception {
        String renderedHost = runContext.render(this.host).as(String.class).orElseThrow();
        String renderedUser = runContext.render(this.user).as(String.class).orElseThrow();
        String renderedPassword = runContext.render(this.password).as(String.class).orElseThrow();

        // Prevent GUI prompts in headless environments (mirrors plugin-jdbc-as400)
        AS400.setDefaultSignonHandler(new NonInteractiveSignonHandler());
        System.setProperty("com.ibm.as400.access.AS400.guiAvailable", "false");

        AS400 system = new AS400(renderedHost, renderedUser, renderedPassword);
        return system;
    }
}
