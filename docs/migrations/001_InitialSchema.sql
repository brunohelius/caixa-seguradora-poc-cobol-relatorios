CREATE TABLE IF NOT EXISTS "__EFMigrationsHistory" (
    "MigrationId" TEXT NOT NULL CONSTRAINT "PK___EFMigrationsHistory" PRIMARY KEY,
    "ProductVersion" TEXT NOT NULL
);

BEGIN TRANSACTION;
CREATE TABLE "Agencies" (
    "AgencyCode" INTEGER NOT NULL CONSTRAINT "PK_Agencies" PRIMARY KEY AUTOINCREMENT,
    "AgencyName" TEXT NOT NULL,
    "RegionalCode" INTEGER NOT NULL,
    "RegionalName" TEXT NOT NULL,
    "Status" TEXT NOT NULL DEFAULT 'A'
);

CREATE TABLE "BatchJobs" (
    "JobId" INTEGER NOT NULL CONSTRAINT "PK_BatchJobs" PRIMARY KEY AUTOINCREMENT,
    "JobName" TEXT NOT NULL,
    "Description" TEXT NOT NULL,
    "RecurrencePattern" TEXT NOT NULL,
    "ReportParameters" text NOT NULL,
    "Status" TEXT NOT NULL DEFAULT 'ACTIVE',
    "NextExecutionTime" TEXT NULL,
    "LastExecutionTime" TEXT NULL,
    "CreatedBy" TEXT NOT NULL,
    "CreatedDate" TEXT NOT NULL,
    "UpdatedDate" TEXT NULL,
    "ExecutionHour" INTEGER NULL,
    "ExecutionMinute" INTEGER NULL,
    "DayOfWeek" INTEGER NULL,
    "DayOfMonth" INTEGER NULL,
    "NotificationRecipients" TEXT NOT NULL,
    "IsEnabled" INTEGER NOT NULL DEFAULT 1,
    "MaxRetries" INTEGER NOT NULL DEFAULT 3,
    "RetryCount" INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE "Clients" (
    "ClientCode" INTEGER NOT NULL CONSTRAINT "PK_Clients" PRIMARY KEY AUTOINCREMENT,
    "CompanyCode" INTEGER NOT NULL,
    "ClientName" TEXT NOT NULL,
    "ClientType" TEXT NOT NULL,
    "DocumentNumber" TEXT NOT NULL,
    "IdentityDocument" TEXT NOT NULL,
    "BirthDate" TEXT NOT NULL,
    "Gender" TEXT NOT NULL,
    "Email" TEXT NOT NULL,
    "PhoneNumber" TEXT NOT NULL,
    "ClientStatus" TEXT NOT NULL DEFAULT 'A'
);

CREATE TABLE "Products" (
    "ProductCode" INTEGER NOT NULL CONSTRAINT "PK_Products" PRIMARY KEY AUTOINCREMENT,
    "CompanyCode" INTEGER NOT NULL,
    "ProductName" TEXT NOT NULL,
    "LineOfBusiness" INTEGER NOT NULL,
    "LineOfBusinessGroup" INTEGER NOT NULL,
    "SusepProcessNumber" TEXT NOT NULL,
    "ProductType" TEXT NOT NULL,
    "ProductStatus" TEXT NOT NULL DEFAULT 'A',
    "ProductModality" INTEGER NOT NULL,
    "IsLifeInsurance" TEXT NOT NULL DEFAULT 'N',
    "CommissionPercentage" decimal(6,2) NOT NULL
);

CREATE TABLE "ReportDefinitions" (
    "Id" INTEGER NOT NULL CONSTRAINT "PK_ReportDefinitions" PRIMARY KEY AUTOINCREMENT,
    "ReportCode" TEXT NOT NULL,
    "ReportName" TEXT NOT NULL,
    "Description" TEXT NOT NULL,
    "OutputFormat" TEXT NOT NULL DEFAULT 'TXT',
    "RecordLength" INTEGER NOT NULL,
    "IsActive" INTEGER NOT NULL DEFAULT 1,
    "CreatedAt" TEXT NOT NULL,
    "UpdatedAt" TEXT NULL
);

CREATE TABLE "SystemConfigurations" (
    "Id" INTEGER NOT NULL CONSTRAINT "PK_SystemConfigurations" PRIMARY KEY AUTOINCREMENT,
    "ConfigKey" TEXT NOT NULL,
    "ConfigValue" TEXT NOT NULL,
    "Description" TEXT NOT NULL,
    "Category" TEXT NOT NULL,
    "CreatedAt" TEXT NOT NULL,
    "UpdatedAt" TEXT NULL
);

CREATE TABLE "Producers" (
    "ProducerCode" INTEGER NOT NULL CONSTRAINT "PK_Producers" PRIMARY KEY AUTOINCREMENT,
    "ProducerName" TEXT NOT NULL,
    "TaxId" TEXT NOT NULL,
    "DefaultCommissionPercentage" decimal(5,2) NOT NULL,
    "Status" TEXT NOT NULL DEFAULT 'A',
    "AgencyId" INTEGER NULL,
    CONSTRAINT "FK_Producers_Agencies_AgencyId" FOREIGN KEY ("AgencyId") REFERENCES "Agencies" ("AgencyCode") ON DELETE SET NULL
);

CREATE TABLE "BatchJobExecutions" (
    "ExecutionId" INTEGER NOT NULL CONSTRAINT "PK_BatchJobExecutions" PRIMARY KEY AUTOINCREMENT,
    "JobId" INTEGER NOT NULL,
    "StartTime" TEXT NOT NULL,
    "EndTime" TEXT NULL,
    "Status" TEXT NOT NULL DEFAULT 'RUNNING',
    "ErrorMessage" TEXT NOT NULL,
    "OutputFilePath" TEXT NOT NULL,
    "RecordsProcessed" INTEGER NOT NULL DEFAULT 0,
    "ExecutedBy" TEXT NOT NULL DEFAULT 'SYSTEM',
    "ExecutionLog" text NOT NULL,
    CONSTRAINT "FK_BatchJobExecutions_BatchJobs_JobId" FOREIGN KEY ("JobId") REFERENCES "BatchJobs" ("JobId") ON DELETE CASCADE
);

CREATE TABLE "Addresses" (
    "AddressId" INTEGER NOT NULL CONSTRAINT "PK_Addresses" PRIMARY KEY AUTOINCREMENT,
    "ClientCode" INTEGER NOT NULL,
    "AddressSequence" INTEGER NOT NULL,
    "AddressType" TEXT NOT NULL,
    "StreetAddress" TEXT NOT NULL,
    "Number" TEXT NOT NULL,
    "Complement" TEXT NOT NULL,
    "Neighborhood" TEXT NOT NULL,
    "City" TEXT NOT NULL,
    "State" TEXT NOT NULL,
    "PostalCode" TEXT NOT NULL,
    "CountryCode" INTEGER NOT NULL,
    "Country" TEXT NOT NULL DEFAULT 'Brasil',
    CONSTRAINT "FK_Addresses_Clients_ClientCode" FOREIGN KEY ("ClientCode") REFERENCES "Clients" ("ClientCode") ON DELETE CASCADE
);

CREATE TABLE "Policies" (
    "PolicyNumber" INTEGER NOT NULL CONSTRAINT "PK_Policies" PRIMARY KEY AUTOINCREMENT,
    "EndorsementNumber" INTEGER NOT NULL,
    "SystemCode" TEXT NOT NULL,
    "ProductCode" INTEGER NOT NULL,
    "EffectiveDate" TEXT NOT NULL,
    "ExpirationDate" TEXT NOT NULL,
    "TotalPremium" decimal(15,2) NOT NULL,
    "NetPremium" decimal(15,2) NOT NULL,
    "PolicyStatus" TEXT NOT NULL,
    "ClientCode" INTEGER NOT NULL,
    "AgencyCode" INTEGER NOT NULL,
    "ProducerCode" INTEGER NOT NULL,
    "InsuredClientCode" INTEGER NOT NULL,
    "InsuredCode" INTEGER NOT NULL,
    "PolicyStartDate" TEXT NOT NULL,
    "PolicyEndDate" TEXT NOT NULL,
    "PolicyStatusCode" TEXT NOT NULL,
    "ProposalNumber" INTEGER NOT NULL,
    "ProductCode1" INTEGER NULL,
    CONSTRAINT "FK_Policies_Agencies_AgencyCode" FOREIGN KEY ("AgencyCode") REFERENCES "Agencies" ("AgencyCode") ON DELETE RESTRICT,
    CONSTRAINT "FK_Policies_Clients_ClientCode" FOREIGN KEY ("ClientCode") REFERENCES "Clients" ("ClientCode") ON DELETE RESTRICT,
    CONSTRAINT "FK_Policies_Producers_ProducerCode" FOREIGN KEY ("ProducerCode") REFERENCES "Producers" ("ProducerCode") ON DELETE RESTRICT,
    CONSTRAINT "FK_Policies_Products_ProductCode" FOREIGN KEY ("ProductCode") REFERENCES "Products" ("ProductCode") ON DELETE RESTRICT,
    CONSTRAINT "FK_Policies_Products_ProductCode1" FOREIGN KEY ("ProductCode1") REFERENCES "Products" ("ProductCode")
);

CREATE TABLE "CossuredPolicies" (
    "CossuranceId" INTEGER NOT NULL CONSTRAINT "PK_CossuredPolicies" PRIMARY KEY AUTOINCREMENT,
    "PolicyNumber" INTEGER NOT NULL,
    "CossuranceCode" INTEGER NOT NULL,
    "CossuranceType" TEXT NOT NULL,
    "CedingCompanyCode" INTEGER NOT NULL,
    "AcquiringCompanyCode" INTEGER NOT NULL,
    "PercentageShare" decimal(13,9) NOT NULL,
    "CededInsuredAmount" decimal(15,2) NOT NULL,
    "CededPremium" decimal(15,2) NOT NULL,
    "IsLeader" TEXT NOT NULL DEFAULT 'N',
    "CossurerCode" INTEGER NOT NULL,
    "CossurerName" TEXT NOT NULL,
    "Status" TEXT NOT NULL DEFAULT 'A',
    CONSTRAINT "FK_CossuredPolicies_Policies_PolicyNumber" FOREIGN KEY ("PolicyNumber") REFERENCES "Policies" ("PolicyNumber") ON DELETE CASCADE
);

CREATE TABLE "Coverages" (
    "Id" INTEGER NOT NULL CONSTRAINT "PK_Coverages" PRIMARY KEY AUTOINCREMENT,
    "CoverageCode" INTEGER NOT NULL,
    "CoverageName" TEXT NOT NULL,
    "InsuredAmount" decimal(15,2) NOT NULL,
    "PremiumAmount" decimal(15,2) NOT NULL,
    "DeductiblePercentage" decimal(5,2) NOT NULL,
    "Status" TEXT NOT NULL DEFAULT 'A',
    "PolicyId" INTEGER NOT NULL,
    "ProductCode" INTEGER NOT NULL,
    "CoverageType" TEXT NOT NULL,
    "PolicyNumber" INTEGER NOT NULL,
    CONSTRAINT "FK_Coverages_Policies_PolicyNumber" FOREIGN KEY ("PolicyNumber") REFERENCES "Policies" ("PolicyNumber") ON DELETE CASCADE,
    CONSTRAINT "FK_Coverages_Products_ProductCode" FOREIGN KEY ("ProductCode") REFERENCES "Products" ("ProductCode") ON DELETE RESTRICT
);

CREATE TABLE "Endorsements" (
    "Id" INTEGER NOT NULL CONSTRAINT "PK_Endorsements" PRIMARY KEY AUTOINCREMENT,
    "EndorsementNumber" INTEGER NOT NULL,
    "EndorsementType" TEXT NOT NULL,
    "IssueDate" TEXT NOT NULL,
    "EffectiveDate" TEXT NOT NULL,
    "PremiumAmount" decimal(15,2) NOT NULL,
    "Reason" TEXT NOT NULL,
    "Status" TEXT NOT NULL DEFAULT 'A',
    "PolicyId" INTEGER NOT NULL,
    "PolicyNumber" INTEGER NOT NULL,
    "CancellationFlag" TEXT NOT NULL,
    CONSTRAINT "FK_Endorsements_Policies_PolicyNumber" FOREIGN KEY ("PolicyNumber") REFERENCES "Policies" ("PolicyNumber") ON DELETE CASCADE
);

CREATE TABLE "Invoices" (
    "Id" INTEGER NOT NULL CONSTRAINT "PK_Invoices" PRIMARY KEY AUTOINCREMENT,
    "InvoiceNumber" INTEGER NOT NULL,
    "IssueDate" TEXT NOT NULL,
    "DueDate" TEXT NOT NULL,
    "TotalAmount" decimal(15,2) NOT NULL,
    "PaidAmount" decimal(15,2) NOT NULL,
    "Status" TEXT NOT NULL DEFAULT 'PE',
    "NumberOfInstallments" INTEGER NOT NULL,
    "PolicyNumber" INTEGER NOT NULL,
    CONSTRAINT "FK_Invoices_Policies_PolicyNumber" FOREIGN KEY ("PolicyNumber") REFERENCES "Policies" ("PolicyNumber") ON DELETE CASCADE
);

CREATE TABLE "CossuranceCalculations" (
    "CalculationId" INTEGER NOT NULL CONSTRAINT "PK_CossuranceCalculations" PRIMARY KEY AUTOINCREMENT,
    "PolicyNumber" INTEGER NOT NULL,
    "CossuranceCode" INTEGER NOT NULL,
    "QuotaPercentage" decimal(13,9) NOT NULL,
    "RetainedPremium" decimal(15,2) NOT NULL,
    "CededPremium" decimal(15,2) NOT NULL,
    "CededCommission" decimal(15,2) NOT NULL,
    "TotalGrossPremium" decimal(15,2) NOT NULL,
    "TotalNetPremium" decimal(15,2) NOT NULL,
    "TotalIOF" decimal(15,2) NOT NULL,
    "CossuredPolicyId" INTEGER NULL,
    CONSTRAINT "FK_CossuranceCalculations_CossuredPolicies_CossuredPolicyId" FOREIGN KEY ("CossuredPolicyId") REFERENCES "CossuredPolicies" ("CossuranceId") ON DELETE CASCADE,
    CONSTRAINT "FK_CossuranceCalculations_Policies_PolicyNumber" FOREIGN KEY ("PolicyNumber") REFERENCES "Policies" ("PolicyNumber") ON DELETE RESTRICT
);

CREATE TABLE "PremiumRecords" (
    "PremiumId" INTEGER NOT NULL CONSTRAINT "PK_PremiumRecords" PRIMARY KEY AUTOINCREMENT,
    "CompanyCode" INTEGER NOT NULL,
    "ReferenceYear" INTEGER NOT NULL,
    "ReferenceMonth" INTEGER NOT NULL,
    "ReferenceDay" INTEGER NOT NULL,
    "MovementType" TEXT NOT NULL,
    "PolicyNumber" INTEGER NOT NULL,
    "EndorsementNumber" INTEGER NOT NULL,
    "InstallmentNumber" INTEGER NOT NULL,
    "OccurrenceNumber" INTEGER NOT NULL,
    "HistoricalOccurrence" INTEGER NOT NULL,
    "LineOfBusiness" INTEGER NOT NULL,
    "ProductModality" INTEGER NOT NULL,
    "OperationType" INTEGER NOT NULL,
    "BusinessOperationType" INTEGER NOT NULL,
    "ClientCode" INTEGER NOT NULL,
    "ExchangeRate" decimal(15,9) NOT NULL,
    "InsuredAmountItem" decimal(15,5) NOT NULL,
    "BasePremiumItem" decimal(15,5) NOT NULL,
    "FixedPremiumItem" decimal(15,5) NOT NULL,
    "TariffPremiumItem" decimal(15,5) NOT NULL,
    "DiscountItem" decimal(15,5) NOT NULL,
    "NetPremiumItem" decimal(15,5) NOT NULL,
    "AdditionalFractionalItem" decimal(15,5) NOT NULL,
    "IssuanceCostItem" decimal(15,5) NOT NULL,
    "IofItem" decimal(15,5) NOT NULL,
    "TotalPremiumItem" decimal(15,5) NOT NULL,
    "CommissionItem" decimal(15,5) NOT NULL,
    "AdministrationFeeItem" decimal(15,5) NOT NULL,
    "AgencyCommissionItem" decimal(15,5) NOT NULL,
    "PreferentialCommissionItem" decimal(15,5) NOT NULL,
    "InsuredAmountNet" decimal(15,5) NOT NULL,
    "BasePremiumNet" decimal(15,5) NOT NULL,
    "FixedPremiumNet" decimal(15,5) NOT NULL,
    "TariffPremiumNet" decimal(15,5) NOT NULL,
    "DiscountNet" decimal(15,5) NOT NULL,
    "NetPremiumNet" decimal(15,5) NOT NULL,
    "AdditionalFractionalNet" decimal(15,5) NOT NULL,
    "IssuanceCostNet" decimal(15,5) NOT NULL,
    "IofNet" decimal(15,5) NOT NULL,
    "TotalPremiumNet" decimal(15,5) NOT NULL,
    "CommissionNet" decimal(15,5) NOT NULL,
    "AdministrationFeeNet" decimal(15,5) NOT NULL,
    "AgencyCommissionNet" decimal(15,5) NOT NULL,
    "PreferentialCommissionNet" decimal(15,5) NOT NULL,
    "InsuredAmountCossurance" decimal(15,5) NOT NULL,
    "BasePremiumCossurance" decimal(15,5) NOT NULL,
    "FixedPremiumCossurance" decimal(15,5) NOT NULL,
    "TariffPremiumCossurance" decimal(15,5) NOT NULL,
    "DiscountCossurance" decimal(15,5) NOT NULL,
    "NetPremiumCossurance" decimal(15,5) NOT NULL,
    "AdditionalFractionalCossurance" decimal(15,5) NOT NULL,
    "CommissionCossurance" decimal(15,5) NOT NULL,
    "AdministrationFeeCossurance" decimal(15,5) NOT NULL,
    "AgencyCommissionCossurance" decimal(15,5) NOT NULL,
    "PreferentialCommissionCossurance" decimal(15,5) NOT NULL,
    "InsuredAmountReinsurance" decimal(15,5) NOT NULL,
    "TariffPremiumReinsurance" decimal(15,5) NOT NULL,
    "DiscountReinsurance" decimal(15,5) NOT NULL,
    "NetPremiumReinsurance" decimal(15,5) NOT NULL,
    "AdditionalFractionalReinsurance" decimal(15,5) NOT NULL,
    "CommissionReinsurance" decimal(15,5) NOT NULL,
    "InsuredAmountTotal" decimal(15,2) NOT NULL,
    "BasePremiumTotal" decimal(15,2) NOT NULL,
    "FixedPremiumTotal" decimal(15,2) NOT NULL,
    "TariffPremiumTotal" decimal(15,2) NOT NULL,
    "DiscountTotal" decimal(15,2) NOT NULL,
    "NetPremiumTotal" decimal(15,2) NOT NULL,
    "AdditionalFractionalTotal" decimal(15,2) NOT NULL,
    "IssuanceCostTotal" decimal(15,2) NOT NULL,
    "IofTotal" decimal(15,2) NOT NULL,
    "TotalPremiumTotal" decimal(15,2) NOT NULL,
    "CommissionTotal" decimal(15,2) NOT NULL,
    "AdministrationFeeTotal" decimal(15,2) NOT NULL,
    "AgencyCommissionTotal" decimal(15,2) NOT NULL,
    "PreferentialCommissionTotal" decimal(15,2) NOT NULL,
    "InsuredAmountLocalTotal" decimal(15,2) NOT NULL,
    "BasePremiumLocalTotal" decimal(15,2) NOT NULL,
    "FixedPremiumLocalTotal" decimal(15,2) NOT NULL,
    "ProductCode" INTEGER NOT NULL,
    "SystemCode" TEXT NOT NULL,
    "Id" INTEGER NOT NULL,
    "AgencyCode" INTEGER NOT NULL,
    "AgencyName" TEXT NOT NULL,
    "ProducerCode" INTEGER NOT NULL,
    "ProducerName" TEXT NOT NULL,
    "ProducerCommissionPercentage" decimal(6,2) NOT NULL,
    "InsuredCode" INTEGER NOT NULL,
    "InsuredName" TEXT NOT NULL,
    "InsuredTaxId" TEXT NOT NULL,
    "InsuredPersonType" TEXT NOT NULL,
    "ProductName" TEXT NOT NULL,
    "PolicyStartDate" TEXT NOT NULL,
    "PolicyStatus" TEXT NOT NULL,
    "CurrencyCode" TEXT NOT NULL,
    "CossuranceIndicator" TEXT NOT NULL,
    "CossurancePercentage" decimal(13,9) NOT NULL,
    "CossurancePremium" decimal(15,2) NOT NULL,
    "Street" TEXT NOT NULL,
    "AddressNumber" TEXT NOT NULL,
    "City" TEXT NOT NULL,
    "State" TEXT NOT NULL,
    "PostalCode" TEXT NOT NULL,
    "CalculationType" TEXT NOT NULL,
    "CreatedBy" TEXT NOT NULL,
    "UpdatedBy" TEXT NOT NULL,
    "RecordChecksum" TEXT NOT NULL,
    "ProductCode1" INTEGER NULL,
    "ClientCode1" INTEGER NULL,
    "EndorsementId" INTEGER NULL,
    CONSTRAINT "FK_PremiumRecords_Clients_ClientCode1" FOREIGN KEY ("ClientCode1") REFERENCES "Clients" ("ClientCode"),
    CONSTRAINT "FK_PremiumRecords_Endorsements_EndorsementId" FOREIGN KEY ("EndorsementId") REFERENCES "Endorsements" ("Id"),
    CONSTRAINT "FK_PremiumRecords_Policies_PolicyNumber" FOREIGN KEY ("PolicyNumber") REFERENCES "Policies" ("PolicyNumber") ON DELETE SET NULL,
    CONSTRAINT "FK_PremiumRecords_Products_ProductCode1" FOREIGN KEY ("ProductCode1") REFERENCES "Products" ("ProductCode")
);

CREATE TABLE "Installments" (
    "Id" INTEGER NOT NULL CONSTRAINT "PK_Installments" PRIMARY KEY AUTOINCREMENT,
    "InstallmentNumber" INTEGER NOT NULL,
    "DueDate" TEXT NOT NULL,
    "InstallmentAmount" decimal(15,2) NOT NULL,
    "PaidAmount" decimal(15,2) NOT NULL,
    "PaymentDate" TEXT NULL,
    "Status" TEXT NOT NULL DEFAULT 'PE',
    "BarcodeNumber" TEXT NOT NULL,
    "InvoiceId" INTEGER NOT NULL,
    CONSTRAINT "FK_Installments_Invoices_InvoiceId" FOREIGN KEY ("InvoiceId") REFERENCES "Invoices" ("Id") ON DELETE CASCADE
);

CREATE INDEX "IX_Addresses_ClientCode" ON "Addresses" ("ClientCode");

CREATE INDEX "IX_BatchJobExecutions_JobId" ON "BatchJobExecutions" ("JobId");

CREATE INDEX "IX_BatchJobExecutions_StartTime" ON "BatchJobExecutions" ("StartTime");

CREATE INDEX "IX_BatchJobExecutions_Status" ON "BatchJobExecutions" ("Status");

CREATE INDEX "IX_BatchJobs_JobName" ON "BatchJobs" ("JobName");

CREATE INDEX "IX_BatchJobs_NextExecutionTime" ON "BatchJobs" ("NextExecutionTime");

CREATE INDEX "IX_BatchJobs_Status_IsEnabled" ON "BatchJobs" ("Status", "IsEnabled");

CREATE UNIQUE INDEX "IX_Clients_DocumentNumber" ON "Clients" ("DocumentNumber");

CREATE INDEX "IX_CossuranceCalculations_CossuredPolicyId" ON "CossuranceCalculations" ("CossuredPolicyId");

CREATE INDEX "IX_CossuranceCalculations_PolicyCossurance" ON "CossuranceCalculations" ("PolicyNumber", "CossuranceCode");

CREATE INDEX "IX_CossuredPolicies_PolicyCossurance" ON "CossuredPolicies" ("PolicyNumber", "CossuranceCode");

CREATE INDEX "IX_Coverages_PolicyNumber" ON "Coverages" ("PolicyNumber");

CREATE INDEX "IX_Coverages_ProductCode" ON "Coverages" ("ProductCode");

CREATE INDEX "IX_Endorsements_PolicyNumber_EndorsementNumber" ON "Endorsements" ("PolicyNumber", "EndorsementNumber");

CREATE INDEX "IX_Installments_InvoiceId_InstallmentNumber" ON "Installments" ("InvoiceId", "InstallmentNumber");

CREATE UNIQUE INDEX "IX_Invoices_InvoiceNumber" ON "Invoices" ("InvoiceNumber");

CREATE INDEX "IX_Invoices_PolicyNumber" ON "Invoices" ("PolicyNumber");

CREATE INDEX "IX_Policies_AgencyCode" ON "Policies" ("AgencyCode");

CREATE INDEX "IX_Policies_ClientCode" ON "Policies" ("ClientCode");

CREATE INDEX "IX_Policies_PolicyNumber_EndorsementNumber" ON "Policies" ("PolicyNumber", "EndorsementNumber");

CREATE INDEX "IX_Policies_ProducerCode" ON "Policies" ("ProducerCode");

CREATE INDEX "IX_Policies_ProductCode" ON "Policies" ("ProductCode");

CREATE INDEX "IX_Policies_ProductCode1" ON "Policies" ("ProductCode1");

CREATE INDEX "IX_PremiumRecords_ClientCode1" ON "PremiumRecords" ("ClientCode1");

CREATE INDEX "IX_PremiumRecords_DateRange" ON "PremiumRecords" ("CompanyCode", "ReferenceYear", "ReferenceMonth", "ReferenceDay");

CREATE INDEX "IX_PremiumRecords_EndorsementId" ON "PremiumRecords" ("EndorsementId");

CREATE INDEX "IX_PremiumRecords_PolicyNumber" ON "PremiumRecords" ("PolicyNumber");

CREATE INDEX "IX_PremiumRecords_PolicyNumber_EndorsementNumber" ON "PremiumRecords" ("PolicyNumber", "EndorsementNumber");

CREATE INDEX "IX_PremiumRecords_ProductCode1" ON "PremiumRecords" ("ProductCode1");

CREATE INDEX "IX_Producers_AgencyId" ON "Producers" ("AgencyId");

CREATE INDEX "IX_Products_LineOfBusiness" ON "Products" ("CompanyCode", "LineOfBusiness");

CREATE INDEX "IX_Products_Status" ON "Products" ("ProductStatus");

CREATE UNIQUE INDEX "IX_ReportDefinitions_ReportCode" ON "ReportDefinitions" ("ReportCode");

CREATE UNIQUE INDEX "IX_SystemConfigurations_ConfigKey" ON "SystemConfigurations" ("ConfigKey");

INSERT INTO "__EFMigrationsHistory" ("MigrationId", "ProductVersion")
VALUES ('20251023101046_InitialCreate', '9.0.10');

ALTER TABLE "Policies" ADD "IssueDate" TEXT NOT NULL DEFAULT '0001-01-01 00:00:00';

ALTER TABLE "Policies" ADD "ProposalDate" TEXT NOT NULL DEFAULT '0001-01-01 00:00:00';

ALTER TABLE "Policies" ADD "ProposerClientCode" INTEGER NOT NULL DEFAULT 0;

ALTER TABLE "Policies" ADD "RamoSusep" INTEGER NOT NULL DEFAULT 0;

ALTER TABLE "Policies" ADD "StateCode" TEXT NOT NULL DEFAULT '';

ALTER TABLE "Endorsements" ADD "EndDate" TEXT NOT NULL DEFAULT '0001-01-01 00:00:00';

CREATE TABLE "REINSURANCE_DATA" (
    "ReinsuranceId" INTEGER NOT NULL CONSTRAINT "PK_REINSURANCE_DATA" PRIMARY KEY AUTOINCREMENT,
    "PolicyNumber" BIGINT NOT NULL,
    "EffectiveDate" TEXT NOT NULL,
    "PremiumAmount" DECIMAL(15,2) NOT NULL,
    "ReinsuredAmount" DECIMAL(15,2) NOT NULL,
    "ReinsurancePercentage" DECIMAL(5,2) NOT NULL,
    "TreatyCode" TEXT NOT NULL,
    "ReturnCode" TEXT NOT NULL,
    "ErrorMessage" TEXT NOT NULL,
    "ProductCode" INTEGER NOT NULL,
    "RamoSusep" INTEGER NOT NULL,
    "CompanyCode" INTEGER NOT NULL,
    "CreatedAt" TEXT NOT NULL,
    "UpdatedAt" TEXT NULL,
    "CreatedBy" TEXT NOT NULL,
    "UpdatedBy" TEXT NOT NULL,
    CONSTRAINT "FK_REINSURANCE_DATA_Policies_PolicyNumber" FOREIGN KEY ("PolicyNumber") REFERENCES "Policies" ("PolicyNumber") ON DELETE RESTRICT
);

CREATE TABLE "REPORT_EXECUTIONS" (
    "ExecutionId" TEXT NOT NULL CONSTRAINT "PK_REPORT_EXECUTIONS" PRIMARY KEY,
    "ReferenceMonth" TEXT NOT NULL,
    "StartTime" TEXT NOT NULL,
    "EndTime" TEXT NULL,
    "Status" TEXT NOT NULL,
    "RecordsProcessed" INTEGER NOT NULL DEFAULT 0,
    "PremitRecordsGenerated" INTEGER NOT NULL DEFAULT 0,
    "PremcedRecordsGenerated" INTEGER NOT NULL DEFAULT 0,
    "WarningsCount" INTEGER NOT NULL DEFAULT 0,
    "ErrorsCount" INTEGER NOT NULL DEFAULT 0,
    "ReturnCode" TEXT NOT NULL,
    "TriggeringUser" TEXT NOT NULL,
    "ReportType" TEXT NOT NULL,
    "ExecutionMode" TEXT NOT NULL
);

CREATE TABLE "FILE_OUTPUTS" (
    "FileId" TEXT NOT NULL CONSTRAINT "PK_FILE_OUTPUTS" PRIMARY KEY,
    "ExecutionId" TEXT NOT NULL,
    "FileName" TEXT NOT NULL,
    "FileType" TEXT NOT NULL,
    "FilePath" TEXT NOT NULL,
    "FileSizeBytes" INTEGER NOT NULL,
    "RecordCount" INTEGER NOT NULL,
    "GeneratedAt" TEXT NOT NULL,
    "Checksum" TEXT NOT NULL,
    "DownloadCount" INTEGER NOT NULL DEFAULT 0,
    CONSTRAINT "FK_FILE_OUTPUTS_REPORT_EXECUTIONS_ExecutionId" FOREIGN KEY ("ExecutionId") REFERENCES "REPORT_EXECUTIONS" ("ExecutionId") ON DELETE CASCADE
);

CREATE TABLE "PROCESSING_LOGS" (
    "LogId" INTEGER NOT NULL CONSTRAINT "PK_PROCESSING_LOGS" PRIMARY KEY AUTOINCREMENT,
    "ExecutionId" TEXT NOT NULL,
    "Timestamp" TEXT NOT NULL,
    "Severity" TEXT NOT NULL,
    "CobolSection" TEXT NOT NULL,
    "PolicyNumber" BIGINT NULL,
    "Message" TEXT NOT NULL,
    "StackTrace" TEXT NULL,
    CONSTRAINT "FK_PROCESSING_LOGS_REPORT_EXECUTIONS_ExecutionId" FOREIGN KEY ("ExecutionId") REFERENCES "REPORT_EXECUTIONS" ("ExecutionId") ON DELETE CASCADE
);

CREATE INDEX "IX_FileOutput_Execution_FileType" ON "FILE_OUTPUTS" ("ExecutionId", "FileType");

CREATE INDEX "IX_FileOutput_ExecutionId" ON "FILE_OUTPUTS" ("ExecutionId");

CREATE INDEX "IX_FileOutput_FileName" ON "FILE_OUTPUTS" ("FileName");

CREATE INDEX "IX_FileOutput_FileType" ON "FILE_OUTPUTS" ("FileType");

CREATE INDEX "IX_FileOutput_GeneratedAt" ON "FILE_OUTPUTS" ("GeneratedAt");

CREATE INDEX "IX_ProcessingLog_CobolSection" ON "PROCESSING_LOGS" ("CobolSection");

CREATE INDEX "IX_ProcessingLog_Execution_Severity" ON "PROCESSING_LOGS" ("ExecutionId", "Severity");

CREATE INDEX "IX_ProcessingLog_Execution_Timestamp" ON "PROCESSING_LOGS" ("ExecutionId", "Timestamp");

CREATE INDEX "IX_ProcessingLog_ExecutionId" ON "PROCESSING_LOGS" ("ExecutionId");

CREATE INDEX "IX_ProcessingLog_PolicyNumber" ON "PROCESSING_LOGS" ("PolicyNumber");

CREATE INDEX "IX_ProcessingLog_Severity" ON "PROCESSING_LOGS" ("Severity");

CREATE INDEX "IX_ProcessingLog_Timestamp" ON "PROCESSING_LOGS" ("Timestamp");

CREATE INDEX "IX_ReinsuranceData_EffectiveDate" ON "REINSURANCE_DATA" ("EffectiveDate");

CREATE INDEX "IX_ReinsuranceData_Policy_EffectiveDate" ON "REINSURANCE_DATA" ("PolicyNumber", "EffectiveDate");

CREATE INDEX "IX_ReinsuranceData_PolicyNumber" ON "REINSURANCE_DATA" ("PolicyNumber");

CREATE INDEX "IX_ReinsuranceData_ReturnCode" ON "REINSURANCE_DATA" ("ReturnCode");

CREATE INDEX "IX_ReinsuranceData_TreatyCode" ON "REINSURANCE_DATA" ("TreatyCode");

CREATE INDEX "IX_ReportExecution_Month_Status" ON "REPORT_EXECUTIONS" ("ReferenceMonth", "Status");

CREATE INDEX "IX_ReportExecution_ReferenceMonth" ON "REPORT_EXECUTIONS" ("ReferenceMonth");

CREATE INDEX "IX_ReportExecution_ReturnCode" ON "REPORT_EXECUTIONS" ("ReturnCode");

CREATE INDEX "IX_ReportExecution_StartTime" ON "REPORT_EXECUTIONS" ("StartTime");

CREATE INDEX "IX_ReportExecution_Status" ON "REPORT_EXECUTIONS" ("Status");

INSERT INTO "__EFMigrationsHistory" ("MigrationId", "ProductVersion")
VALUES ('20251027225829_InitialSchema', '9.0.10');

COMMIT;

