using System;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace CaixaSeguradora.Infrastructure.Migrations
{
    /// <inheritdoc />
    public partial class InitialCreate : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "Agencies",
                columns: table => new
                {
                    AgencyCode = table.Column<int>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    AgencyName = table.Column<string>(type: "TEXT", maxLength: 60, nullable: false),
                    RegionalCode = table.Column<int>(type: "INTEGER", nullable: false),
                    RegionalName = table.Column<string>(type: "TEXT", maxLength: 50, nullable: false),
                    Status = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false, defaultValue: "A")
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Agencies", x => x.AgencyCode);
                });

            migrationBuilder.CreateTable(
                name: "BatchJobs",
                columns: table => new
                {
                    JobId = table.Column<int>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    JobName = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    Description = table.Column<string>(type: "TEXT", maxLength: 500, nullable: false),
                    RecurrencePattern = table.Column<string>(type: "TEXT", maxLength: 20, nullable: false),
                    ReportParameters = table.Column<string>(type: "text", nullable: false),
                    Status = table.Column<string>(type: "TEXT", maxLength: 20, nullable: false, defaultValue: "ACTIVE"),
                    NextExecutionTime = table.Column<DateTime>(type: "TEXT", nullable: true),
                    LastExecutionTime = table.Column<DateTime>(type: "TEXT", nullable: true),
                    CreatedBy = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    CreatedDate = table.Column<DateTime>(type: "TEXT", nullable: false),
                    UpdatedDate = table.Column<DateTime>(type: "TEXT", nullable: true),
                    ExecutionHour = table.Column<int>(type: "INTEGER", nullable: true),
                    ExecutionMinute = table.Column<int>(type: "INTEGER", nullable: true),
                    DayOfWeek = table.Column<int>(type: "INTEGER", nullable: true),
                    DayOfMonth = table.Column<int>(type: "INTEGER", nullable: true),
                    NotificationRecipients = table.Column<string>(type: "TEXT", maxLength: 500, nullable: false),
                    IsEnabled = table.Column<bool>(type: "INTEGER", nullable: false, defaultValue: true),
                    MaxRetries = table.Column<int>(type: "INTEGER", nullable: false, defaultValue: 3),
                    RetryCount = table.Column<int>(type: "INTEGER", nullable: false, defaultValue: 0)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_BatchJobs", x => x.JobId);
                });

            migrationBuilder.CreateTable(
                name: "Clients",
                columns: table => new
                {
                    ClientCode = table.Column<int>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    CompanyCode = table.Column<int>(type: "INTEGER", nullable: false),
                    ClientName = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    ClientType = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false),
                    DocumentNumber = table.Column<string>(type: "TEXT", maxLength: 14, nullable: false),
                    IdentityDocument = table.Column<string>(type: "TEXT", maxLength: 20, nullable: false),
                    BirthDate = table.Column<string>(type: "TEXT", maxLength: 10, nullable: false),
                    Gender = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false),
                    Email = table.Column<string>(type: "TEXT", maxLength: 50, nullable: false),
                    PhoneNumber = table.Column<string>(type: "TEXT", maxLength: 20, nullable: false),
                    ClientStatus = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false, defaultValue: "A")
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Clients", x => x.ClientCode);
                });

            migrationBuilder.CreateTable(
                name: "Products",
                columns: table => new
                {
                    ProductCode = table.Column<int>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    CompanyCode = table.Column<int>(type: "INTEGER", nullable: false),
                    ProductName = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    LineOfBusiness = table.Column<int>(type: "INTEGER", nullable: false),
                    LineOfBusinessGroup = table.Column<int>(type: "INTEGER", nullable: false),
                    SusepProcessNumber = table.Column<string>(type: "TEXT", maxLength: 20, nullable: false),
                    ProductType = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false),
                    ProductStatus = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false, defaultValue: "A"),
                    ProductModality = table.Column<int>(type: "INTEGER", nullable: false),
                    IsLifeInsurance = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false, defaultValue: "N"),
                    CommissionPercentage = table.Column<decimal>(type: "decimal(6,2)", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Products", x => x.ProductCode);
                });

            migrationBuilder.CreateTable(
                name: "ReportDefinitions",
                columns: table => new
                {
                    Id = table.Column<int>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    ReportCode = table.Column<string>(type: "TEXT", maxLength: 10, nullable: false),
                    ReportName = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    Description = table.Column<string>(type: "TEXT", maxLength: 200, nullable: false),
                    OutputFormat = table.Column<string>(type: "TEXT", maxLength: 10, nullable: false, defaultValue: "TXT"),
                    RecordLength = table.Column<int>(type: "INTEGER", nullable: false),
                    IsActive = table.Column<bool>(type: "INTEGER", nullable: false, defaultValue: true),
                    CreatedAt = table.Column<DateTime>(type: "TEXT", nullable: false),
                    UpdatedAt = table.Column<DateTime>(type: "TEXT", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_ReportDefinitions", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "SystemConfigurations",
                columns: table => new
                {
                    Id = table.Column<int>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    ConfigKey = table.Column<string>(type: "TEXT", maxLength: 50, nullable: false),
                    ConfigValue = table.Column<string>(type: "TEXT", maxLength: 500, nullable: false),
                    Description = table.Column<string>(type: "TEXT", maxLength: 200, nullable: false),
                    Category = table.Column<string>(type: "TEXT", maxLength: 50, nullable: false),
                    CreatedAt = table.Column<DateTime>(type: "TEXT", nullable: false),
                    UpdatedAt = table.Column<DateTime>(type: "TEXT", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_SystemConfigurations", x => x.Id);
                });

            migrationBuilder.CreateTable(
                name: "Producers",
                columns: table => new
                {
                    ProducerCode = table.Column<int>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    ProducerName = table.Column<string>(type: "TEXT", maxLength: 60, nullable: false),
                    TaxId = table.Column<string>(type: "TEXT", maxLength: 11, nullable: false),
                    DefaultCommissionPercentage = table.Column<decimal>(type: "decimal(5,2)", nullable: false),
                    Status = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false, defaultValue: "A"),
                    AgencyId = table.Column<int>(type: "INTEGER", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Producers", x => x.ProducerCode);
                    table.ForeignKey(
                        name: "FK_Producers_Agencies_AgencyId",
                        column: x => x.AgencyId,
                        principalTable: "Agencies",
                        principalColumn: "AgencyCode",
                        onDelete: ReferentialAction.SetNull);
                });

            migrationBuilder.CreateTable(
                name: "BatchJobExecutions",
                columns: table => new
                {
                    ExecutionId = table.Column<int>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    JobId = table.Column<int>(type: "INTEGER", nullable: false),
                    StartTime = table.Column<DateTime>(type: "TEXT", nullable: false),
                    EndTime = table.Column<DateTime>(type: "TEXT", nullable: true),
                    Status = table.Column<string>(type: "TEXT", maxLength: 20, nullable: false, defaultValue: "RUNNING"),
                    ErrorMessage = table.Column<string>(type: "TEXT", maxLength: 1000, nullable: false),
                    OutputFilePath = table.Column<string>(type: "TEXT", maxLength: 500, nullable: false),
                    RecordsProcessed = table.Column<int>(type: "INTEGER", nullable: false, defaultValue: 0),
                    ExecutedBy = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false, defaultValue: "SYSTEM"),
                    ExecutionLog = table.Column<string>(type: "text", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_BatchJobExecutions", x => x.ExecutionId);
                    table.ForeignKey(
                        name: "FK_BatchJobExecutions_BatchJobs_JobId",
                        column: x => x.JobId,
                        principalTable: "BatchJobs",
                        principalColumn: "JobId",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "Addresses",
                columns: table => new
                {
                    AddressId = table.Column<long>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    ClientCode = table.Column<int>(type: "INTEGER", nullable: false),
                    AddressSequence = table.Column<int>(type: "INTEGER", nullable: false),
                    AddressType = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false),
                    StreetAddress = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    Number = table.Column<string>(type: "TEXT", maxLength: 10, nullable: false),
                    Complement = table.Column<string>(type: "TEXT", maxLength: 50, nullable: false),
                    Neighborhood = table.Column<string>(type: "TEXT", maxLength: 50, nullable: false),
                    City = table.Column<string>(type: "TEXT", maxLength: 50, nullable: false),
                    State = table.Column<string>(type: "TEXT", maxLength: 2, nullable: false),
                    PostalCode = table.Column<string>(type: "TEXT", maxLength: 8, nullable: false),
                    CountryCode = table.Column<int>(type: "INTEGER", nullable: false),
                    Country = table.Column<string>(type: "TEXT", maxLength: 50, nullable: false, defaultValue: "Brasil")
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Addresses", x => x.AddressId);
                    table.ForeignKey(
                        name: "FK_Addresses_Clients_ClientCode",
                        column: x => x.ClientCode,
                        principalTable: "Clients",
                        principalColumn: "ClientCode",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "Policies",
                columns: table => new
                {
                    PolicyNumber = table.Column<long>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    EndorsementNumber = table.Column<int>(type: "INTEGER", nullable: false),
                    SystemCode = table.Column<string>(type: "TEXT", maxLength: 2, nullable: false),
                    ProductCode = table.Column<int>(type: "INTEGER", nullable: false),
                    EffectiveDate = table.Column<DateTime>(type: "TEXT", nullable: false),
                    ExpirationDate = table.Column<DateTime>(type: "TEXT", nullable: false),
                    TotalPremium = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    NetPremium = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    PolicyStatus = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false),
                    ClientCode = table.Column<int>(type: "INTEGER", nullable: false),
                    AgencyCode = table.Column<int>(type: "INTEGER", nullable: false),
                    ProducerCode = table.Column<int>(type: "INTEGER", nullable: false),
                    InsuredClientCode = table.Column<int>(type: "INTEGER", nullable: false),
                    InsuredCode = table.Column<int>(type: "INTEGER", nullable: false),
                    PolicyStartDate = table.Column<string>(type: "TEXT", nullable: false),
                    PolicyEndDate = table.Column<string>(type: "TEXT", nullable: false),
                    PolicyStatusCode = table.Column<string>(type: "TEXT", nullable: false),
                    ProposalNumber = table.Column<long>(type: "INTEGER", nullable: false),
                    ProductCode1 = table.Column<int>(type: "INTEGER", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Policies", x => x.PolicyNumber);
                    table.ForeignKey(
                        name: "FK_Policies_Agencies_AgencyCode",
                        column: x => x.AgencyCode,
                        principalTable: "Agencies",
                        principalColumn: "AgencyCode",
                        onDelete: ReferentialAction.Restrict);
                    table.ForeignKey(
                        name: "FK_Policies_Clients_ClientCode",
                        column: x => x.ClientCode,
                        principalTable: "Clients",
                        principalColumn: "ClientCode",
                        onDelete: ReferentialAction.Restrict);
                    table.ForeignKey(
                        name: "FK_Policies_Producers_ProducerCode",
                        column: x => x.ProducerCode,
                        principalTable: "Producers",
                        principalColumn: "ProducerCode",
                        onDelete: ReferentialAction.Restrict);
                    table.ForeignKey(
                        name: "FK_Policies_Products_ProductCode",
                        column: x => x.ProductCode,
                        principalTable: "Products",
                        principalColumn: "ProductCode",
                        onDelete: ReferentialAction.Restrict);
                    table.ForeignKey(
                        name: "FK_Policies_Products_ProductCode1",
                        column: x => x.ProductCode1,
                        principalTable: "Products",
                        principalColumn: "ProductCode");
                });

            migrationBuilder.CreateTable(
                name: "CossuredPolicies",
                columns: table => new
                {
                    CossuranceId = table.Column<long>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    PolicyNumber = table.Column<long>(type: "INTEGER", nullable: false),
                    CossuranceCode = table.Column<int>(type: "INTEGER", nullable: false),
                    CossuranceType = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false),
                    CedingCompanyCode = table.Column<int>(type: "INTEGER", nullable: false),
                    AcquiringCompanyCode = table.Column<int>(type: "INTEGER", nullable: false),
                    PercentageShare = table.Column<decimal>(type: "decimal(13,9)", nullable: false),
                    CededInsuredAmount = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    CededPremium = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    IsLeader = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false, defaultValue: "N"),
                    CossurerCode = table.Column<int>(type: "INTEGER", nullable: false),
                    CossurerName = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    Status = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false, defaultValue: "A")
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_CossuredPolicies", x => x.CossuranceId);
                    table.ForeignKey(
                        name: "FK_CossuredPolicies_Policies_PolicyNumber",
                        column: x => x.PolicyNumber,
                        principalTable: "Policies",
                        principalColumn: "PolicyNumber",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "Coverages",
                columns: table => new
                {
                    Id = table.Column<int>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    CoverageCode = table.Column<int>(type: "INTEGER", nullable: false),
                    CoverageName = table.Column<string>(type: "TEXT", maxLength: 50, nullable: false),
                    InsuredAmount = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    PremiumAmount = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    DeductiblePercentage = table.Column<decimal>(type: "decimal(5,2)", nullable: false),
                    Status = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false, defaultValue: "A"),
                    PolicyId = table.Column<long>(type: "INTEGER", nullable: false),
                    ProductCode = table.Column<int>(type: "INTEGER", nullable: false),
                    CoverageType = table.Column<string>(type: "TEXT", nullable: false),
                    PolicyNumber = table.Column<long>(type: "INTEGER", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Coverages", x => x.Id);
                    table.ForeignKey(
                        name: "FK_Coverages_Policies_PolicyNumber",
                        column: x => x.PolicyNumber,
                        principalTable: "Policies",
                        principalColumn: "PolicyNumber",
                        onDelete: ReferentialAction.Cascade);
                    table.ForeignKey(
                        name: "FK_Coverages_Products_ProductCode",
                        column: x => x.ProductCode,
                        principalTable: "Products",
                        principalColumn: "ProductCode",
                        onDelete: ReferentialAction.Restrict);
                });

            migrationBuilder.CreateTable(
                name: "Endorsements",
                columns: table => new
                {
                    Id = table.Column<int>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    EndorsementNumber = table.Column<int>(type: "INTEGER", nullable: false),
                    EndorsementType = table.Column<string>(type: "TEXT", maxLength: 2, nullable: false),
                    IssueDate = table.Column<DateTime>(type: "TEXT", nullable: false),
                    EffectiveDate = table.Column<DateTime>(type: "TEXT", nullable: false),
                    PremiumAmount = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    Reason = table.Column<string>(type: "TEXT", maxLength: 200, nullable: false),
                    Status = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false, defaultValue: "A"),
                    PolicyId = table.Column<long>(type: "INTEGER", nullable: false),
                    PolicyNumber = table.Column<long>(type: "INTEGER", nullable: false),
                    CancellationFlag = table.Column<string>(type: "TEXT", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Endorsements", x => x.Id);
                    table.ForeignKey(
                        name: "FK_Endorsements_Policies_PolicyNumber",
                        column: x => x.PolicyNumber,
                        principalTable: "Policies",
                        principalColumn: "PolicyNumber",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "Invoices",
                columns: table => new
                {
                    Id = table.Column<int>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    InvoiceNumber = table.Column<long>(type: "INTEGER", nullable: false),
                    IssueDate = table.Column<DateTime>(type: "TEXT", nullable: false),
                    DueDate = table.Column<DateTime>(type: "TEXT", nullable: false),
                    TotalAmount = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    PaidAmount = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    Status = table.Column<string>(type: "TEXT", maxLength: 2, nullable: false, defaultValue: "PE"),
                    NumberOfInstallments = table.Column<int>(type: "INTEGER", nullable: false),
                    PolicyNumber = table.Column<long>(type: "INTEGER", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Invoices", x => x.Id);
                    table.ForeignKey(
                        name: "FK_Invoices_Policies_PolicyNumber",
                        column: x => x.PolicyNumber,
                        principalTable: "Policies",
                        principalColumn: "PolicyNumber",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "CossuranceCalculations",
                columns: table => new
                {
                    CalculationId = table.Column<long>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    PolicyNumber = table.Column<long>(type: "INTEGER", nullable: false),
                    CossuranceCode = table.Column<int>(type: "INTEGER", nullable: false),
                    QuotaPercentage = table.Column<decimal>(type: "decimal(13,9)", nullable: false),
                    RetainedPremium = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    CededPremium = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    CededCommission = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    TotalGrossPremium = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    TotalNetPremium = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    TotalIOF = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    CossuredPolicyId = table.Column<long>(type: "INTEGER", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_CossuranceCalculations", x => x.CalculationId);
                    table.ForeignKey(
                        name: "FK_CossuranceCalculations_CossuredPolicies_CossuredPolicyId",
                        column: x => x.CossuredPolicyId,
                        principalTable: "CossuredPolicies",
                        principalColumn: "CossuranceId",
                        onDelete: ReferentialAction.Cascade);
                    table.ForeignKey(
                        name: "FK_CossuranceCalculations_Policies_PolicyNumber",
                        column: x => x.PolicyNumber,
                        principalTable: "Policies",
                        principalColumn: "PolicyNumber",
                        onDelete: ReferentialAction.Restrict);
                });

            migrationBuilder.CreateTable(
                name: "PremiumRecords",
                columns: table => new
                {
                    PremiumId = table.Column<long>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    CompanyCode = table.Column<int>(type: "INTEGER", nullable: false),
                    ReferenceYear = table.Column<int>(type: "INTEGER", nullable: false),
                    ReferenceMonth = table.Column<int>(type: "INTEGER", nullable: false),
                    ReferenceDay = table.Column<int>(type: "INTEGER", nullable: false),
                    MovementType = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false),
                    PolicyNumber = table.Column<long>(type: "INTEGER", nullable: false),
                    EndorsementNumber = table.Column<int>(type: "INTEGER", nullable: false),
                    InstallmentNumber = table.Column<int>(type: "INTEGER", nullable: false),
                    OccurrenceNumber = table.Column<int>(type: "INTEGER", nullable: false),
                    HistoricalOccurrence = table.Column<int>(type: "INTEGER", nullable: false),
                    LineOfBusiness = table.Column<int>(type: "INTEGER", nullable: false),
                    ProductModality = table.Column<int>(type: "INTEGER", nullable: false),
                    OperationType = table.Column<int>(type: "INTEGER", nullable: false),
                    BusinessOperationType = table.Column<int>(type: "INTEGER", nullable: false),
                    ClientCode = table.Column<int>(type: "INTEGER", nullable: false),
                    ExchangeRate = table.Column<decimal>(type: "decimal(15,9)", nullable: false),
                    InsuredAmountItem = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    BasePremiumItem = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    FixedPremiumItem = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    TariffPremiumItem = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    DiscountItem = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    NetPremiumItem = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    AdditionalFractionalItem = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    IssuanceCostItem = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    IofItem = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    TotalPremiumItem = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    CommissionItem = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    AdministrationFeeItem = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    AgencyCommissionItem = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    PreferentialCommissionItem = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    InsuredAmountNet = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    BasePremiumNet = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    FixedPremiumNet = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    TariffPremiumNet = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    DiscountNet = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    NetPremiumNet = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    AdditionalFractionalNet = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    IssuanceCostNet = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    IofNet = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    TotalPremiumNet = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    CommissionNet = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    AdministrationFeeNet = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    AgencyCommissionNet = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    PreferentialCommissionNet = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    InsuredAmountCossurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    BasePremiumCossurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    FixedPremiumCossurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    TariffPremiumCossurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    DiscountCossurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    NetPremiumCossurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    AdditionalFractionalCossurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    CommissionCossurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    AdministrationFeeCossurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    AgencyCommissionCossurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    PreferentialCommissionCossurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    InsuredAmountReinsurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    TariffPremiumReinsurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    DiscountReinsurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    NetPremiumReinsurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    AdditionalFractionalReinsurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    CommissionReinsurance = table.Column<decimal>(type: "decimal(15,5)", nullable: false),
                    InsuredAmountTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    BasePremiumTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    FixedPremiumTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    TariffPremiumTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    DiscountTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    NetPremiumTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    AdditionalFractionalTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    IssuanceCostTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    IofTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    TotalPremiumTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    CommissionTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    AdministrationFeeTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    AgencyCommissionTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    PreferentialCommissionTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    InsuredAmountLocalTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    BasePremiumLocalTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    FixedPremiumLocalTotal = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    ProductCode = table.Column<int>(type: "INTEGER", nullable: false),
                    SystemCode = table.Column<string>(type: "TEXT", maxLength: 2, nullable: false),
                    Id = table.Column<long>(type: "INTEGER", nullable: false),
                    AgencyCode = table.Column<int>(type: "INTEGER", nullable: false),
                    AgencyName = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    ProducerCode = table.Column<int>(type: "INTEGER", nullable: false),
                    ProducerName = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    ProducerCommissionPercentage = table.Column<decimal>(type: "decimal(6,2)", nullable: false),
                    InsuredCode = table.Column<int>(type: "INTEGER", nullable: false),
                    InsuredName = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    InsuredTaxId = table.Column<string>(type: "TEXT", maxLength: 14, nullable: false),
                    InsuredPersonType = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false),
                    ProductName = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    PolicyStartDate = table.Column<string>(type: "TEXT", maxLength: 10, nullable: false),
                    PolicyStatus = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false),
                    CurrencyCode = table.Column<string>(type: "TEXT", maxLength: 3, nullable: false),
                    CossuranceIndicator = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false),
                    CossurancePercentage = table.Column<decimal>(type: "decimal(13,9)", nullable: false),
                    CossurancePremium = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    Street = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    AddressNumber = table.Column<string>(type: "TEXT", maxLength: 10, nullable: false),
                    City = table.Column<string>(type: "TEXT", maxLength: 50, nullable: false),
                    State = table.Column<string>(type: "TEXT", maxLength: 2, nullable: false),
                    PostalCode = table.Column<string>(type: "TEXT", maxLength: 8, nullable: false),
                    CalculationType = table.Column<string>(type: "TEXT", maxLength: 1, nullable: false),
                    CreatedBy = table.Column<string>(type: "TEXT", maxLength: 8, nullable: false),
                    UpdatedBy = table.Column<string>(type: "TEXT", maxLength: 8, nullable: false),
                    RecordChecksum = table.Column<string>(type: "TEXT", maxLength: 32, nullable: false),
                    ProductCode1 = table.Column<int>(type: "INTEGER", nullable: true),
                    ClientCode1 = table.Column<int>(type: "INTEGER", nullable: true),
                    EndorsementId = table.Column<int>(type: "INTEGER", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_PremiumRecords", x => x.PremiumId);
                    table.ForeignKey(
                        name: "FK_PremiumRecords_Clients_ClientCode1",
                        column: x => x.ClientCode1,
                        principalTable: "Clients",
                        principalColumn: "ClientCode");
                    table.ForeignKey(
                        name: "FK_PremiumRecords_Endorsements_EndorsementId",
                        column: x => x.EndorsementId,
                        principalTable: "Endorsements",
                        principalColumn: "Id");
                    table.ForeignKey(
                        name: "FK_PremiumRecords_Policies_PolicyNumber",
                        column: x => x.PolicyNumber,
                        principalTable: "Policies",
                        principalColumn: "PolicyNumber",
                        onDelete: ReferentialAction.SetNull);
                    table.ForeignKey(
                        name: "FK_PremiumRecords_Products_ProductCode1",
                        column: x => x.ProductCode1,
                        principalTable: "Products",
                        principalColumn: "ProductCode");
                });

            migrationBuilder.CreateTable(
                name: "Installments",
                columns: table => new
                {
                    Id = table.Column<int>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    InstallmentNumber = table.Column<int>(type: "INTEGER", nullable: false),
                    DueDate = table.Column<DateTime>(type: "TEXT", nullable: false),
                    InstallmentAmount = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    PaidAmount = table.Column<decimal>(type: "decimal(15,2)", nullable: false),
                    PaymentDate = table.Column<DateTime>(type: "TEXT", nullable: true),
                    Status = table.Column<string>(type: "TEXT", maxLength: 2, nullable: false, defaultValue: "PE"),
                    BarcodeNumber = table.Column<string>(type: "TEXT", maxLength: 47, nullable: false),
                    InvoiceId = table.Column<int>(type: "INTEGER", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Installments", x => x.Id);
                    table.ForeignKey(
                        name: "FK_Installments_Invoices_InvoiceId",
                        column: x => x.InvoiceId,
                        principalTable: "Invoices",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateIndex(
                name: "IX_Addresses_ClientCode",
                table: "Addresses",
                column: "ClientCode");

            migrationBuilder.CreateIndex(
                name: "IX_BatchJobExecutions_JobId",
                table: "BatchJobExecutions",
                column: "JobId");

            migrationBuilder.CreateIndex(
                name: "IX_BatchJobExecutions_StartTime",
                table: "BatchJobExecutions",
                column: "StartTime");

            migrationBuilder.CreateIndex(
                name: "IX_BatchJobExecutions_Status",
                table: "BatchJobExecutions",
                column: "Status");

            migrationBuilder.CreateIndex(
                name: "IX_BatchJobs_JobName",
                table: "BatchJobs",
                column: "JobName");

            migrationBuilder.CreateIndex(
                name: "IX_BatchJobs_NextExecutionTime",
                table: "BatchJobs",
                column: "NextExecutionTime");

            migrationBuilder.CreateIndex(
                name: "IX_BatchJobs_Status_IsEnabled",
                table: "BatchJobs",
                columns: new[] { "Status", "IsEnabled" });

            migrationBuilder.CreateIndex(
                name: "IX_Clients_DocumentNumber",
                table: "Clients",
                column: "DocumentNumber",
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_CossuranceCalculations_CossuredPolicyId",
                table: "CossuranceCalculations",
                column: "CossuredPolicyId");

            migrationBuilder.CreateIndex(
                name: "IX_CossuranceCalculations_PolicyCossurance",
                table: "CossuranceCalculations",
                columns: new[] { "PolicyNumber", "CossuranceCode" });

            migrationBuilder.CreateIndex(
                name: "IX_CossuredPolicies_PolicyCossurance",
                table: "CossuredPolicies",
                columns: new[] { "PolicyNumber", "CossuranceCode" });

            migrationBuilder.CreateIndex(
                name: "IX_Coverages_PolicyNumber",
                table: "Coverages",
                column: "PolicyNumber");

            migrationBuilder.CreateIndex(
                name: "IX_Coverages_ProductCode",
                table: "Coverages",
                column: "ProductCode");

            migrationBuilder.CreateIndex(
                name: "IX_Endorsements_PolicyNumber_EndorsementNumber",
                table: "Endorsements",
                columns: new[] { "PolicyNumber", "EndorsementNumber" });

            migrationBuilder.CreateIndex(
                name: "IX_Installments_InvoiceId_InstallmentNumber",
                table: "Installments",
                columns: new[] { "InvoiceId", "InstallmentNumber" });

            migrationBuilder.CreateIndex(
                name: "IX_Invoices_InvoiceNumber",
                table: "Invoices",
                column: "InvoiceNumber",
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_Invoices_PolicyNumber",
                table: "Invoices",
                column: "PolicyNumber");

            migrationBuilder.CreateIndex(
                name: "IX_Policies_AgencyCode",
                table: "Policies",
                column: "AgencyCode");

            migrationBuilder.CreateIndex(
                name: "IX_Policies_ClientCode",
                table: "Policies",
                column: "ClientCode");

            migrationBuilder.CreateIndex(
                name: "IX_Policies_PolicyNumber_EndorsementNumber",
                table: "Policies",
                columns: new[] { "PolicyNumber", "EndorsementNumber" });

            migrationBuilder.CreateIndex(
                name: "IX_Policies_ProducerCode",
                table: "Policies",
                column: "ProducerCode");

            migrationBuilder.CreateIndex(
                name: "IX_Policies_ProductCode",
                table: "Policies",
                column: "ProductCode");

            migrationBuilder.CreateIndex(
                name: "IX_Policies_ProductCode1",
                table: "Policies",
                column: "ProductCode1");

            migrationBuilder.CreateIndex(
                name: "IX_PremiumRecords_ClientCode1",
                table: "PremiumRecords",
                column: "ClientCode1");

            migrationBuilder.CreateIndex(
                name: "IX_PremiumRecords_DateRange",
                table: "PremiumRecords",
                columns: new[] { "CompanyCode", "ReferenceYear", "ReferenceMonth", "ReferenceDay" });

            migrationBuilder.CreateIndex(
                name: "IX_PremiumRecords_EndorsementId",
                table: "PremiumRecords",
                column: "EndorsementId");

            migrationBuilder.CreateIndex(
                name: "IX_PremiumRecords_PolicyNumber",
                table: "PremiumRecords",
                column: "PolicyNumber");

            migrationBuilder.CreateIndex(
                name: "IX_PremiumRecords_PolicyNumber_EndorsementNumber",
                table: "PremiumRecords",
                columns: new[] { "PolicyNumber", "EndorsementNumber" });

            migrationBuilder.CreateIndex(
                name: "IX_PremiumRecords_ProductCode1",
                table: "PremiumRecords",
                column: "ProductCode1");

            migrationBuilder.CreateIndex(
                name: "IX_Producers_AgencyId",
                table: "Producers",
                column: "AgencyId");

            migrationBuilder.CreateIndex(
                name: "IX_Products_LineOfBusiness",
                table: "Products",
                columns: new[] { "CompanyCode", "LineOfBusiness" });

            migrationBuilder.CreateIndex(
                name: "IX_Products_Status",
                table: "Products",
                column: "ProductStatus");

            migrationBuilder.CreateIndex(
                name: "IX_ReportDefinitions_ReportCode",
                table: "ReportDefinitions",
                column: "ReportCode",
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_SystemConfigurations_ConfigKey",
                table: "SystemConfigurations",
                column: "ConfigKey",
                unique: true);
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "Addresses");

            migrationBuilder.DropTable(
                name: "BatchJobExecutions");

            migrationBuilder.DropTable(
                name: "CossuranceCalculations");

            migrationBuilder.DropTable(
                name: "Coverages");

            migrationBuilder.DropTable(
                name: "Installments");

            migrationBuilder.DropTable(
                name: "PremiumRecords");

            migrationBuilder.DropTable(
                name: "ReportDefinitions");

            migrationBuilder.DropTable(
                name: "SystemConfigurations");

            migrationBuilder.DropTable(
                name: "BatchJobs");

            migrationBuilder.DropTable(
                name: "CossuredPolicies");

            migrationBuilder.DropTable(
                name: "Invoices");

            migrationBuilder.DropTable(
                name: "Endorsements");

            migrationBuilder.DropTable(
                name: "Policies");

            migrationBuilder.DropTable(
                name: "Clients");

            migrationBuilder.DropTable(
                name: "Producers");

            migrationBuilder.DropTable(
                name: "Products");

            migrationBuilder.DropTable(
                name: "Agencies");
        }
    }
}
