package io.kestra.plugin.cobol;

import io.kestra.core.junit.annotations.KestraTest;
import io.kestra.core.models.property.Property;
import io.kestra.core.runners.RunContextFactory;
import jakarta.inject.Inject;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.time.Duration;
import java.util.List;
import java.util.Map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Integration tests against a local "mainframe simulator" TCP stack.
 *
 * The simulator is started by .github/setup-unit.sh through docker-compose-ci.yml.
 * It does not implement IBM i protocol; it only guarantees network reachability so we can
 * validate real JTOpen transport/protocol failures without Mockito.
 */
@KestraTest
@Tag("integration")
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@EnabledIfEnvironmentVariable(named = "COBOL_INTEGRATION_TESTS", matches = "true")
class MainframeSimulatorIntegrationTest {
    private static final String HOST = "127.0.0.1";
    private static final List<Integer> MAINFRAME_PORTS = List.of(8470, 8471, 8472, 8473, 8474, 8475, 8476);

    @Inject
    private RunContextFactory runContextFactory;

    @BeforeAll
    void shouldReachSimulatorPorts() throws Exception {
        for (var port : MAINFRAME_PORTS) {
            try (var socket = new Socket()) {
                socket.connect(new InetSocketAddress(HOST, port), 1000);
            }
        }
    }

    @Test
    void submitJobShouldFailWithTransportOrProtocolError() {
        var runContext = runContextFactory.of(Map.of());
        var task = SubmitJob.builder()
            .host(Property.ofValue(HOST))
            .user(Property.ofValue("TESTUSER"))
            .password(Property.ofValue("TESTPASS"))
            .library(Property.ofValue("BATCHLIB"))
            .program(Property.ofValue("EODPROC"))
            .build();

        assertProtocolFailure(() -> task.run(runContext));
    }

    @Test
    void createProgramShouldFailWithTransportOrProtocolError() {
        var runContext = runContextFactory.of(Map.of());
        var task = CreateProgram.builder()
            .host(Property.ofValue(HOST))
            .user(Property.ofValue("TESTUSER"))
            .password(Property.ofValue("TESTPASS"))
            .library(Property.ofValue("DEVLIB"))
            .program(Property.ofValue("HELLO"))
            .sourceInline(Property.ofValue("IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO."))
            .build();

        assertProtocolFailure(() -> task.run(runContext));
    }

    @Test
    void callJobShouldFailWithTransportOrProtocolError() {
        var runContext = runContextFactory.of(Map.of());
        var task = CallJob.builder()
            .host(Property.ofValue(HOST))
            .user(Property.ofValue("TESTUSER"))
            .password(Property.ofValue("TESTPASS"))
            .library(Property.ofValue("FINLIB"))
            .program(Property.ofValue("CALCINT"))
            .build();

        assertProtocolFailure(() -> task.run(runContext));
    }

    private void assertProtocolFailure(ThrowingTaskCall taskCall) {
        var start = System.nanoTime();
        var exception = assertThrows(Exception.class, taskCall::run);
        var executionDuration = Duration.ofNanos(System.nanoTime() - start);

        assertThat(exception, not(instanceOf(IllegalArgumentException.class)));
        assertThat(isProtocolOrTransportError(exception), is(true));
        assertThat(executionDuration.compareTo(Duration.ofSeconds(20)) < 0, is(true));
    }

    private boolean isProtocolOrTransportError(Throwable throwable) {
        var current = throwable;
        while (current != null) {
            if (current instanceof IOException || current.getClass().getName().startsWith("com.ibm.as400.access.")) {
                return true;
            }
            current = current.getCause();
        }
        return false;
    }

    @FunctionalInterface
    private interface ThrowingTaskCall {
        void run() throws Exception;
    }
}
