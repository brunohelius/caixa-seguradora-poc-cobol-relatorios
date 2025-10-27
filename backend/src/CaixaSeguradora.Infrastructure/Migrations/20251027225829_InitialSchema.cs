using System;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace CaixaSeguradora.Infrastructure.Migrations
{
    /// <inheritdoc />
    public partial class InitialSchema : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.AddColumn<DateTime>(
                name: "IssueDate",
                table: "Policies",
                type: "TEXT",
                nullable: false,
                defaultValue: new DateTime(1, 1, 1, 0, 0, 0, 0, DateTimeKind.Unspecified));

            migrationBuilder.AddColumn<DateTime>(
                name: "ProposalDate",
                table: "Policies",
                type: "TEXT",
                nullable: false,
                defaultValue: new DateTime(1, 1, 1, 0, 0, 0, 0, DateTimeKind.Unspecified));

            migrationBuilder.AddColumn<int>(
                name: "ProposerClientCode",
                table: "Policies",
                type: "INTEGER",
                nullable: false,
                defaultValue: 0);

            migrationBuilder.AddColumn<int>(
                name: "RamoSusep",
                table: "Policies",
                type: "INTEGER",
                nullable: false,
                defaultValue: 0);

            migrationBuilder.AddColumn<string>(
                name: "StateCode",
                table: "Policies",
                type: "TEXT",
                nullable: false,
                defaultValue: "");

            migrationBuilder.AddColumn<DateTime>(
                name: "EndDate",
                table: "Endorsements",
                type: "TEXT",
                nullable: false,
                defaultValue: new DateTime(1, 1, 1, 0, 0, 0, 0, DateTimeKind.Unspecified));

            migrationBuilder.CreateTable(
                name: "REINSURANCE_DATA",
                columns: table => new
                {
                    ReinsuranceId = table.Column<long>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    PolicyNumber = table.Column<long>(type: "BIGINT", nullable: false),
                    EffectiveDate = table.Column<DateTime>(type: "TEXT", nullable: false),
                    PremiumAmount = table.Column<decimal>(type: "DECIMAL(15,2)", precision: 15, scale: 2, nullable: false),
                    ReinsuredAmount = table.Column<decimal>(type: "DECIMAL(15,2)", precision: 15, scale: 2, nullable: false),
                    ReinsurancePercentage = table.Column<decimal>(type: "DECIMAL(5,2)", precision: 5, scale: 2, nullable: false),
                    TreatyCode = table.Column<string>(type: "TEXT", maxLength: 10, nullable: false),
                    ReturnCode = table.Column<string>(type: "TEXT", maxLength: 2, nullable: false),
                    ErrorMessage = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    ProductCode = table.Column<short>(type: "INTEGER", nullable: false),
                    RamoSusep = table.Column<int>(type: "INTEGER", nullable: false),
                    CompanyCode = table.Column<short>(type: "INTEGER", nullable: false),
                    CreatedAt = table.Column<DateTime>(type: "TEXT", nullable: false),
                    UpdatedAt = table.Column<DateTime>(type: "TEXT", nullable: true),
                    CreatedBy = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    UpdatedBy = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_REINSURANCE_DATA", x => x.ReinsuranceId);
                    table.ForeignKey(
                        name: "FK_REINSURANCE_DATA_Policies_PolicyNumber",
                        column: x => x.PolicyNumber,
                        principalTable: "Policies",
                        principalColumn: "PolicyNumber",
                        onDelete: ReferentialAction.Restrict);
                });

            migrationBuilder.CreateTable(
                name: "REPORT_EXECUTIONS",
                columns: table => new
                {
                    ExecutionId = table.Column<Guid>(type: "TEXT", nullable: false),
                    ReferenceMonth = table.Column<string>(type: "TEXT", fixedLength: true, maxLength: 6, nullable: false),
                    StartTime = table.Column<DateTime>(type: "TEXT", nullable: false),
                    EndTime = table.Column<DateTime>(type: "TEXT", nullable: true),
                    Status = table.Column<string>(type: "TEXT", maxLength: 20, nullable: false),
                    RecordsProcessed = table.Column<int>(type: "INTEGER", nullable: false, defaultValue: 0),
                    PremitRecordsGenerated = table.Column<int>(type: "INTEGER", nullable: false, defaultValue: 0),
                    PremcedRecordsGenerated = table.Column<int>(type: "INTEGER", nullable: false, defaultValue: 0),
                    WarningsCount = table.Column<int>(type: "INTEGER", nullable: false, defaultValue: 0),
                    ErrorsCount = table.Column<int>(type: "INTEGER", nullable: false, defaultValue: 0),
                    ReturnCode = table.Column<string>(type: "TEXT", maxLength: 4, nullable: false),
                    TriggeringUser = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    ReportType = table.Column<string>(type: "TEXT", maxLength: 10, nullable: false),
                    ExecutionMode = table.Column<string>(type: "TEXT", maxLength: 20, nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_REPORT_EXECUTIONS", x => x.ExecutionId);
                });

            migrationBuilder.CreateTable(
                name: "FILE_OUTPUTS",
                columns: table => new
                {
                    FileId = table.Column<Guid>(type: "TEXT", nullable: false),
                    ExecutionId = table.Column<Guid>(type: "TEXT", nullable: false),
                    FileName = table.Column<string>(type: "TEXT", maxLength: 100, nullable: false),
                    FileType = table.Column<string>(type: "TEXT", maxLength: 20, nullable: false),
                    FilePath = table.Column<string>(type: "TEXT", nullable: false),
                    FileSizeBytes = table.Column<long>(type: "INTEGER", nullable: false),
                    RecordCount = table.Column<int>(type: "INTEGER", nullable: false),
                    GeneratedAt = table.Column<DateTime>(type: "TEXT", nullable: false),
                    Checksum = table.Column<string>(type: "TEXT", maxLength: 64, nullable: false),
                    DownloadCount = table.Column<int>(type: "INTEGER", nullable: false, defaultValue: 0)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_FILE_OUTPUTS", x => x.FileId);
                    table.ForeignKey(
                        name: "FK_FILE_OUTPUTS_REPORT_EXECUTIONS_ExecutionId",
                        column: x => x.ExecutionId,
                        principalTable: "REPORT_EXECUTIONS",
                        principalColumn: "ExecutionId",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "PROCESSING_LOGS",
                columns: table => new
                {
                    LogId = table.Column<long>(type: "INTEGER", nullable: false)
                        .Annotation("Sqlite:Autoincrement", true),
                    ExecutionId = table.Column<Guid>(type: "TEXT", nullable: false),
                    Timestamp = table.Column<DateTime>(type: "TEXT", nullable: false),
                    Severity = table.Column<string>(type: "TEXT", maxLength: 10, nullable: false),
                    CobolSection = table.Column<string>(type: "TEXT", maxLength: 50, nullable: false),
                    PolicyNumber = table.Column<long>(type: "BIGINT", nullable: true),
                    Message = table.Column<string>(type: "TEXT", nullable: false),
                    StackTrace = table.Column<string>(type: "TEXT", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_PROCESSING_LOGS", x => x.LogId);
                    table.ForeignKey(
                        name: "FK_PROCESSING_LOGS_REPORT_EXECUTIONS_ExecutionId",
                        column: x => x.ExecutionId,
                        principalTable: "REPORT_EXECUTIONS",
                        principalColumn: "ExecutionId",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateIndex(
                name: "IX_FileOutput_Execution_FileType",
                table: "FILE_OUTPUTS",
                columns: new[] { "ExecutionId", "FileType" });

            migrationBuilder.CreateIndex(
                name: "IX_FileOutput_ExecutionId",
                table: "FILE_OUTPUTS",
                column: "ExecutionId");

            migrationBuilder.CreateIndex(
                name: "IX_FileOutput_FileName",
                table: "FILE_OUTPUTS",
                column: "FileName");

            migrationBuilder.CreateIndex(
                name: "IX_FileOutput_FileType",
                table: "FILE_OUTPUTS",
                column: "FileType");

            migrationBuilder.CreateIndex(
                name: "IX_FileOutput_GeneratedAt",
                table: "FILE_OUTPUTS",
                column: "GeneratedAt");

            migrationBuilder.CreateIndex(
                name: "IX_ProcessingLog_CobolSection",
                table: "PROCESSING_LOGS",
                column: "CobolSection");

            migrationBuilder.CreateIndex(
                name: "IX_ProcessingLog_Execution_Severity",
                table: "PROCESSING_LOGS",
                columns: new[] { "ExecutionId", "Severity" });

            migrationBuilder.CreateIndex(
                name: "IX_ProcessingLog_Execution_Timestamp",
                table: "PROCESSING_LOGS",
                columns: new[] { "ExecutionId", "Timestamp" });

            migrationBuilder.CreateIndex(
                name: "IX_ProcessingLog_ExecutionId",
                table: "PROCESSING_LOGS",
                column: "ExecutionId");

            migrationBuilder.CreateIndex(
                name: "IX_ProcessingLog_PolicyNumber",
                table: "PROCESSING_LOGS",
                column: "PolicyNumber");

            migrationBuilder.CreateIndex(
                name: "IX_ProcessingLog_Severity",
                table: "PROCESSING_LOGS",
                column: "Severity");

            migrationBuilder.CreateIndex(
                name: "IX_ProcessingLog_Timestamp",
                table: "PROCESSING_LOGS",
                column: "Timestamp");

            migrationBuilder.CreateIndex(
                name: "IX_ReinsuranceData_EffectiveDate",
                table: "REINSURANCE_DATA",
                column: "EffectiveDate");

            migrationBuilder.CreateIndex(
                name: "IX_ReinsuranceData_Policy_EffectiveDate",
                table: "REINSURANCE_DATA",
                columns: new[] { "PolicyNumber", "EffectiveDate" });

            migrationBuilder.CreateIndex(
                name: "IX_ReinsuranceData_PolicyNumber",
                table: "REINSURANCE_DATA",
                column: "PolicyNumber");

            migrationBuilder.CreateIndex(
                name: "IX_ReinsuranceData_ReturnCode",
                table: "REINSURANCE_DATA",
                column: "ReturnCode");

            migrationBuilder.CreateIndex(
                name: "IX_ReinsuranceData_TreatyCode",
                table: "REINSURANCE_DATA",
                column: "TreatyCode");

            migrationBuilder.CreateIndex(
                name: "IX_ReportExecution_Month_Status",
                table: "REPORT_EXECUTIONS",
                columns: new[] { "ReferenceMonth", "Status" });

            migrationBuilder.CreateIndex(
                name: "IX_ReportExecution_ReferenceMonth",
                table: "REPORT_EXECUTIONS",
                column: "ReferenceMonth");

            migrationBuilder.CreateIndex(
                name: "IX_ReportExecution_ReturnCode",
                table: "REPORT_EXECUTIONS",
                column: "ReturnCode");

            migrationBuilder.CreateIndex(
                name: "IX_ReportExecution_StartTime",
                table: "REPORT_EXECUTIONS",
                column: "StartTime");

            migrationBuilder.CreateIndex(
                name: "IX_ReportExecution_Status",
                table: "REPORT_EXECUTIONS",
                column: "Status");
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "FILE_OUTPUTS");

            migrationBuilder.DropTable(
                name: "PROCESSING_LOGS");

            migrationBuilder.DropTable(
                name: "REINSURANCE_DATA");

            migrationBuilder.DropTable(
                name: "REPORT_EXECUTIONS");

            migrationBuilder.DropColumn(
                name: "IssueDate",
                table: "Policies");

            migrationBuilder.DropColumn(
                name: "ProposalDate",
                table: "Policies");

            migrationBuilder.DropColumn(
                name: "ProposerClientCode",
                table: "Policies");

            migrationBuilder.DropColumn(
                name: "RamoSusep",
                table: "Policies");

            migrationBuilder.DropColumn(
                name: "StateCode",
                table: "Policies");

            migrationBuilder.DropColumn(
                name: "EndDate",
                table: "Endorsements");
        }
    }
}
