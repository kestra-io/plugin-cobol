package io.kestra.plugin.cobol;

import io.kestra.core.junit.annotations.KestraTest;
import io.kestra.core.models.property.Property;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

/**
 * Unit tests for CreateProgram task construction and property resolution.
 * Integration tests against a real IBM i system are not included here.
 */
@KestraTest
class CreateProgramTest {

    @Test
    void buildTaskWithInlineSource() {
        CreateProgram task = CreateProgram.builder()
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
            .build();

        assertThat(task.getHost(), notNullValue());
        assertThat(task.getLibrary(), notNullValue());
        assertThat(task.getProgram(), notNullValue());
        assertThat(task.getSourceInline(), notNullValue());
        assertThat(task.getSourceUri(), nullValue());
    }

    @Test
    void buildTaskWithUri() {
        CreateProgram task = CreateProgram.builder()
            .host(Property.ofValue("ibmi.example.com"))
            .user(Property.ofValue("TESTUSER"))
            .password(Property.ofValue("TESTPASS"))
            .library(Property.ofValue("FINLIB"))
            .program(Property.ofValue("CALCINT"))
            .sourceUri(Property.ofValue("https://repo.mybank.com/cobol/CALCINT.cbl"))
            .compileOptions(Property.ofValue("DBGVIEW(*ALL)"))
            .build();

        assertThat(task.getHost(), notNullValue());
        assertThat(task.getLibrary(), notNullValue());
        assertThat(task.getProgram(), notNullValue());
        assertThat(task.getSourceInline(), nullValue());
        assertThat(task.getSourceUri(), notNullValue());
        assertThat(task.getCompileOptions(), notNullValue());
    }
}
