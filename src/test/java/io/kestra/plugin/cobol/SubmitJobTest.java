package io.kestra.plugin.cobol;

import com.ibm.as400.access.AS400;
import com.ibm.as400.access.AS400Message;
import com.ibm.as400.access.CommandCall;
import io.kestra.core.junit.annotations.KestraTest;
import io.kestra.core.models.property.Property;
import io.kestra.core.runners.RunContext;
import io.kestra.core.runners.RunContextFactory;
import jakarta.inject.Inject;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;

import java.util.List;
import java.util.Map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Behavioral tests for SubmitJob run semantics.
 */
@KestraTest
class SubmitJobTest {
    @Inject
    private RunContextFactory runContextFactory;

    @Test
    void runShouldParseSubmittedJobAndEscapeParameters() throws Exception {
        var runContext = runContextFactory.of(Map.of());

        var task = spy(SubmitJob.builder()
            .host(Property.ofValue("ibmi.example.com"))
            .user(Property.ofValue("TESTUSER"))
            .password(Property.ofValue("TESTPASS"))
            .library(Property.ofValue("BATCHLIB"))
            .program(Property.ofValue("EODPROC"))
            .parameters(Property.ofValue(List.of("O'BRIEN", "2026-01-31")))
            .jobName(Property.ofValue("EODJOB"))
            .jobQueue(Property.ofValue("QBATCH"))
            .userProfile(Property.ofValue("BATCHUSER"))
            .build());
        var system = mock(AS400.class);
        doReturn(system).when(task).connect(any(RunContext.class));

        var submitMessage = mock(AS400Message.class);
        when(submitMessage.getID()).thenReturn("CPC1221");
        when(submitMessage.getText()).thenReturn("Job 123456/BATCHUSER/EODJOB submitted to job queue QBATCH in library QGPL.");
        when(submitMessage.getSeverity()).thenReturn(0);

        try (MockedConstruction<CommandCall> mockedCommandCall = mockConstruction(CommandCall.class, (commandCall, context) -> {
            when(commandCall.run(anyString())).thenReturn(true);
            when(commandCall.getMessageList()).thenReturn(new AS400Message[]{submitMessage});
        })) {
            var output = task.run(runContext);

            var createdCommandCall = mockedCommandCall.constructed().getFirst();
            var commandCaptor = ArgumentCaptor.forClass(String.class);
            verify(createdCommandCall).run(commandCaptor.capture());

            assertThat(commandCaptor.getValue(), containsString("PARM('O''BRIEN' '2026-01-31')"));
            assertThat(output.getJobName(), is("EODJOB"));
            assertThat(output.getJobNumber(), is("123456"));
            assertThat(output.getJobUser(), is("BATCHUSER"));
        }
    }

    @Test
    void runShouldRejectInvalidLibraryName() {
        var runContext = runContextFactory.of(Map.of());

        var task = SubmitJob.builder()
            .host(Property.ofValue("ibmi.example.com"))
            .user(Property.ofValue("TESTUSER"))
            .password(Property.ofValue("TESTPASS"))
            .library(Property.ofValue("BATCHLIB) USER(HACKER"))
            .program(Property.ofValue("EODPROC"))
            .build();

        var exception = assertThrows(IllegalArgumentException.class, () -> task.run(runContext));
        assertThat(exception.getMessage(), containsString("library"));
    }
}
