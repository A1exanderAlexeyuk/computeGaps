-- SQL template for creating prevalence analysis results table
-- This file can be used as a reference for manual table creation

CREATE TABLE @cohort_database_schema.@results_table_name (
    result_id BIGINT IDENTITY(1,1) PRIMARY KEY,
    cohortname VARCHAR(255) NOT NULL,
    omop_object_domain VARCHAR(50) NOT NULL,
    object_custom_name VARCHAR(255) NOT NULL,
    workflow_stage VARCHAR(100) NOT NULL,
    concept_id_1 BIGINT NOT NULL,
    concept_id_2 BIGINT NOT NULL,
    n_patients BIGINT NOT NULL,
    n_patients_with_op BIGINT NOT NULL,
    patient_count DECIMAL(5,2) NOT NULL,
    days_around_index_1 INT NOT NULL,
    days_around_index_2 INT NOT NULL,
    analysis_date DATE NOT NULL,
    created_timestamp DATETIME2 DEFAULT GETDATE()
);

-- Create indexes for better query performance
CREATE INDEX IX_@results_table_name_cohortname ON @cohort_database_schema.@results_table_name (cohortname);
CREATE INDEX IX_@results_table_name_domain ON @cohort_database_schema.@results_table_name (omop_object_domain);
CREATE INDEX IX_@results_table_name_workflow ON @cohort_database_schema.@results_table_name (workflow_stage);
CREATE INDEX IX_@results_table_name_concepts ON @cohort_database_schema.@results_table_name (concept_id_1, concept_id_2);
CREATE INDEX IX_@results_table_name_analysis_date ON @cohort_database_schema.@results_table_name (analysis_date);
CREATE INDEX IX_@results_table_name_prevalence ON @cohort_database_schema.@results_table_name (patient_count);

-- Add comments for documentation
EXEC sp_addextendedproperty 
    @name = N'MS_Description', 
    @value = N'Prevalence analysis results table storing test utilization patterns', 
    @level0type = N'SCHEMA', @level0name = '@cohort_database_schema',
    @level1type = N'TABLE', @level1name = '@results_table_name';

EXEC sp_addextendedproperty 
    @name = N'MS_Description', 
    @value = N'Prevalence percentage of test/procedure in cohort', 
    @level0type = N'SCHEMA', @level0name = '@cohort_database_schema',
    @level1type = N'TABLE', @level1name = '@results_table_name',
    @level2type = N'COLUMN', @level2name = 'patient_count';