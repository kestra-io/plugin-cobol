package io.kestra.plugin.cobol;

import io.kestra.core.junit.annotations.KestraTest;
import io.kestra.core.models.property.Property;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

/**
 * Unit tests for CallJob task construction and property resolution.
 * Integration tests against a real IBM i system are not included here.
 */
@KestraTest
class CallJobTest {

    @Test
    void buildTask() {
        CallJob task = CallJob.builder()
            .host(Property.ofValue("ibmi.example.com"))
            .user(Property.ofValue("TESTUSER"))
            .password(Property.ofValue("TESTPASS"))
            .library(Property.ofValue("MYLIB"))
            .program(Property.ofValue("MYPGM"))
            .parameters(Property.ofValue(List.of("PARAM1", "PARAM2")))
            .programTimeout(Property.ofValue(30))
            .build();

        assertThat(task.getHost(), notNullValue());
        assertThat(task.getLibrary(), notNullValue());
        assertThat(task.getProgram(), notNullValue());
        assertThat(task.getParameters(), notNullValue());
        assertThat(task.getProgramTimeout(), notNullValue());
    }

    @Test
    void buildTaskMinimal() {
        CallJob task = CallJob.builder()
            .host(Property.ofValue("ibmi.example.com"))
            .user(Property.ofValue("TESTUSER"))
            .password(Property.ofValue("TESTPASS"))
            .library(Property.ofValue("MYLIB"))
            .program(Property.ofValue("MYPGM"))
            .build();

        assertThat(task.getHost(), notNullValue());
        assertThat(task.getLibrary(), notNullValue());
        assertThat(task.getProgram(), notNullValue());
        assertThat(task.getProgramTimeout(), nullValue());
    }
}
