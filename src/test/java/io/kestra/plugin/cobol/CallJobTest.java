package io.kestra.plugin.cobol;

import java.util.Map;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import com.ibm.as400.access.AS400;
import com.ibm.as400.access.AS400Message;
import com.ibm.as400.access.Job;
import com.ibm.as400.access.ProgramCall;
import com.ibm.as400.access.ProgramParameter;

import io.kestra.core.junit.annotations.KestraTest;
import io.kestra.core.models.property.Property;
import io.kestra.core.runners.RunContext;
import io.kestra.core.runners.RunContextFactory;

import jakarta.inject.Inject;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Behavioral tests for CallJob run semantics.
 */
@KestraTest
class CallJobTest {
    @Inject
    private RunContextFactory runContextFactory;

    @Test
    void runShouldReturnMessagesJobAndDuration() throws Exception {
        var runContext = runContextFactory.of(Map.of());

        var task = spy(
            CallJob.builder()
                .host(Property.ofValue("ibmi.example.com"))
                .user(Property.ofValue("TESTUSER"))
                .password(Property.ofValue("TESTPASS"))
                .library(Property.ofValue("MYLIB"))
                .program(Property.ofValue("MYPGM"))
                .programTimeout(Property.ofValue(30))
                .build()
        );
        var system = mock(AS400.class);
        doReturn(system).when(task).connect(any(RunContext.class));

        var message = mock(AS400Message.class);
        when(message.getID()).thenReturn("CPF0000");
        when(message.getText()).thenReturn("Done");
        when(message.getSeverity()).thenReturn(0);

        var serverJob = mock(Job.class);
        when(serverJob.getName()).thenReturn("QZRCSRVS");
        when(serverJob.getNumber()).thenReturn("123456");
        when(serverJob.getUser()).thenReturn("TESTUSER");

        try (MockedConstruction<ProgramCall> mockedProgramCall = mockConstruction(ProgramCall.class, (programCall, context) ->
        {
            when(programCall.run()).thenReturn(true);
            when(programCall.getMessageList()).thenReturn(new AS400Message[] { message });
            when(programCall.getServerJob()).thenReturn(serverJob);
        })) {
            var output = task.run(runContext);

            var createdProgramCall = mockedProgramCall.constructed().getFirst();
            verify(createdProgramCall).setProgram(anyString(), any(ProgramParameter[].class));
            verify(createdProgramCall).setTimeOut(anyInt());

            assertThat(output.getMessages(), hasSize(1));
            assertThat(output.getJobName(), is("QZRCSRVS"));
            assertThat(output.getJobNumber(), is("123456"));
            assertThat(output.getJobUser(), is("TESTUSER"));
            assertThat(output.getDuration().isNegative(), is(false));
        }
    }

    @Test
    void runShouldFailWhenProgramCallReturnsFalse() throws Exception {
        var runContext = runContextFactory.of(Map.of());

        var task = spy(
            CallJob.builder()
                .host(Property.ofValue("ibmi.example.com"))
                .user(Property.ofValue("TESTUSER"))
                .password(Property.ofValue("TESTPASS"))
                .library(Property.ofValue("MYLIB"))
                .program(Property.ofValue("MYPGM"))
                .build()
        );
        var system = mock(AS400.class);
        doReturn(system).when(task).connect(any(RunContext.class));

        var message = mock(AS400Message.class);
        when(message.getID()).thenReturn("CPF0001");
        when(message.getText()).thenReturn("Program failed");
        when(message.getSeverity()).thenReturn(40);

        try (MockedConstruction<ProgramCall> ignored = mockConstruction(ProgramCall.class, (programCall, context) ->
        {
            when(programCall.run()).thenReturn(false);
            when(programCall.getMessageList()).thenReturn(new AS400Message[] { message });
        })) {
            var exception = assertThrows(IllegalStateException.class, () -> task.run(runContext));
            assertThat(exception.getMessage(), is("Program call failed: CPF0001: Program failed"));
        }
    }
}
