package io.kestra.plugin.cobol;

import io.kestra.core.junit.annotations.KestraTest;
import io.kestra.core.models.property.Property;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

/**
 * Unit tests for SubmitJob task construction and property resolution.
 * Integration tests against a real IBM i system are not included here.
 */
@KestraTest
class SubmitJobTest {

    @Test
    void buildTask() {
        SubmitJob task = SubmitJob.builder()
            .host(Property.ofValue("ibmi.example.com"))
            .user(Property.ofValue("TESTUSER"))
            .password(Property.ofValue("TESTPASS"))
            .library(Property.ofValue("BATCHLIB"))
            .program(Property.ofValue("EODPROC"))
            .parameters(Property.ofValue(List.of("2026-01-31")))
            .jobName(Property.ofValue("EODJOB"))
            .jobQueue(Property.ofValue("QBATCH"))
            .userProfile(Property.ofValue("BATCHUSER"))
            .build();

        assertThat(task.getHost(), notNullValue());
        assertThat(task.getLibrary(), notNullValue());
        assertThat(task.getProgram(), notNullValue());
        assertThat(task.getParameters(), notNullValue());
        assertThat(task.getJobName(), notNullValue());
        assertThat(task.getJobQueue(), notNullValue());
        assertThat(task.getUserProfile(), notNullValue());
    }

    @Test
    void buildTaskMinimal() {
        SubmitJob task = SubmitJob.builder()
            .host(Property.ofValue("ibmi.example.com"))
            .user(Property.ofValue("TESTUSER"))
            .password(Property.ofValue("TESTPASS"))
            .library(Property.ofValue("BATCHLIB"))
            .program(Property.ofValue("EODPROC"))
            .build();

        assertThat(task.getHost(), notNullValue());
        assertThat(task.getLibrary(), notNullValue());
        assertThat(task.getProgram(), notNullValue());
        assertThat(task.getJobName(), nullValue());
        assertThat(task.getJobQueue(), nullValue());
        assertThat(task.getUserProfile(), nullValue());
    }
}
