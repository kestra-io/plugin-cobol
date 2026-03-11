package io.kestra.plugin.cobol;

import java.util.Map;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;

import com.ibm.as400.access.AS400;
import com.ibm.as400.access.AS400Message;
import com.ibm.as400.access.CommandCall;
import com.ibm.as400.access.IFSFileOutputStream;

import io.kestra.core.junit.annotations.KestraTest;
import io.kestra.core.models.property.Property;
import io.kestra.core.runners.RunContext;
import io.kestra.core.runners.RunContextFactory;

import jakarta.inject.Inject;

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
 * Behavioral tests for CreateProgram run semantics.
 */
@KestraTest
class CreateProgramTest {
    @Inject
    private RunContextFactory runContextFactory;

    @Test
    void runShouldCompileInlineSource() throws Exception {
        var runContext = runContextFactory.of(Map.of());

        var task = spy(
            CreateProgram.builder()
                .host(Property.ofValue("ibmi.example.com"))
                .user(Property.ofValue("TESTUSER"))
                .password(Property.ofValue("TESTPASS"))
                .library(Property.ofValue("DEVLIB"))
                .program(Property.ofValue("HELLO"))
                .sourceInline(Property.ofValue("""
                           IDENTIFICATION DIVISION.
                           PROGRAM-ID. HELLO.
                               PROCEDURE DIVISION.
                                   DISPLAY 'HELLO FROM KESTRA'.
                                   STOP RUN.
                    """))
                .compileOptions(Property.ofValue("DBGVIEW(*ALL)"))
                .build()
        );
        var system = mock(AS400.class);
        doReturn(system).when(task).connect(any(RunContext.class));

        var compileMessage = mock(AS400Message.class);
        when(compileMessage.getID()).thenReturn("CPC0000");
        when(compileMessage.getText()).thenReturn("Program created");
        when(compileMessage.getSeverity()).thenReturn(0);

        try (
            MockedConstruction<IFSFileOutputStream> ignoredOutputStream = mockConstruction(IFSFileOutputStream.class);
            MockedConstruction<CommandCall> mockedCommandCall = mockConstruction(CommandCall.class, (commandCall, context) ->
            {
                when(commandCall.run(anyString())).thenReturn(true);
                when(commandCall.getMessageList()).thenReturn(new AS400Message[] { compileMessage });
            })
        ) {
            var output = task.run(runContext);

            var createdCommandCall = mockedCommandCall.constructed().getFirst();
            var commandCaptor = ArgumentCaptor.forClass(String.class);
            verify(createdCommandCall).run(commandCaptor.capture());

            assertThat(commandCaptor.getValue(), containsString("CRTCBLPGM PGM(DEVLIB/HELLO)"));
            assertThat(commandCaptor.getValue(), containsString("DBGVIEW(*ALL)"));
            assertThat(output.getProgramPath(), is("/QSYS.LIB/DEVLIB.LIB/HELLO.PGM"));
            assertThat(output.getCompileMessages().size(), is(1));
        }
    }

    @Test
    void runShouldRejectInvalidCompileOptions() throws Exception {
        var runContext = runContextFactory.of(Map.of());

        var task = spy(
            CreateProgram.builder()
                .host(Property.ofValue("ibmi.example.com"))
                .user(Property.ofValue("TESTUSER"))
                .password(Property.ofValue("TESTPASS"))
                .library(Property.ofValue("FINLIB"))
                .program(Property.ofValue("CALCINT"))
                .sourceInline(Property.ofValue("IDENTIFICATION DIVISION."))
                .compileOptions(Property.ofValue("DBGVIEW(*ALL) ; DLTLIB(MYLIB)"))
                .build()
        );
        var system = mock(AS400.class);
        doReturn(system).when(task).connect(any(RunContext.class));

        try (MockedConstruction<IFSFileOutputStream> ignoredOutputStream = mockConstruction(IFSFileOutputStream.class)) {
            var exception = assertThrows(IllegalArgumentException.class, () -> task.run(runContext));
            assertThat(exception.getMessage(), containsString("compileOptions"));
        }
    }
}
