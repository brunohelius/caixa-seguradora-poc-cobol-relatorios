using System;
using System.Collections.Generic;

namespace PdfGenerator.Models
{
    /// <summary>
    /// Metrics extracted from COBOL program analysis
    /// </summary>
    public class CobolMetrics
    {
        // Program identification
        public string ProgramId { get; set; } = "RG1866B";
        public string ProgramName { get; set; } = "Sistema de Apuração de Prêmios SUSEP";
        public string OriginalLanguage { get; set; } = "COBOL";
        public string TargetPlatform { get; set; } = ".NET 9.0";

        // Code metrics
        public int TotalLines { get; set; } = 4977;
        public int CodeLines { get; set; } = 3982;
        public int CommentLines { get; set; } = 995;
        public int BlankLines { get; set; } = 0;

        // Data complexity
        public int DataItems { get; set; } = 687;
        public int TablesAccessed { get; set; } = 26;
        public int RecordTypes { get; set; } = 31;
        public int TotalFieldsProcessed { get; set; } = 687;

        // Program structure
        public List<CobolDivision> Divisions { get; set; } = new List<CobolDivision>();
        public List<CobolSection> Sections { get; set; } = new List<CobolSection>();
        public List<CobolParagraph> Paragraphs { get; set; } = new List<CobolParagraph>();

        // Database interactions
        public List<DatabaseTable> DatabaseTables { get; set; } = new List<DatabaseTable>();
        public int SqlStatements { get; set; } = 85;
        public int CursorOperations { get; set; } = 12;

        // Business rules
        public int ConditionalStatements { get; set; } = 156;
        public int LoopConstructs { get; set; } = 28;
        public int CalculationOperations { get; set; } = 234;

        // File operations
        public List<FileOperation> FileOperations { get; set; } = new List<FileOperation>();
        public int OutputFiles { get; set; } = 2; // PREMIT.TXT, PREMCED.TXT
        public int InputFiles { get; set; } = 0;

        // Complexity metrics
        public int CyclomaticComplexity { get; set; } = 312;
        public string ComplexityLevel { get; set; } = "High";
        public int EstimatedMigrationEffort { get; set; } = 480; // hours
    }

    public class CobolDivision
    {
        public string Name { get; set; }
        public int StartLine { get; set; }
        public int EndLine { get; set; }
        public int LineCount => EndLine - StartLine + 1;
    }

    public class CobolSection
    {
        public string Name { get; set; }
        public string Division { get; set; }
        public int StartLine { get; set; }
        public int EndLine { get; set; }
        public List<string> Operations { get; set; } = new List<string>();
    }

    public class CobolParagraph
    {
        public string Name { get; set; }
        public string Section { get; set; }
        public int StartLine { get; set; }
        public int EndLine { get; set; }
        public string Purpose { get; set; }
        public List<string> CalledBy { get; set; } = new List<string>();
        public List<string> Calls { get; set; } = new List<string>();
    }

    public class DatabaseTable
    {
        public string TableName { get; set; }
        public string Alias { get; set; }
        public string AccessType { get; set; } // READ, WRITE, UPDATE
        public int FieldCount { get; set; }
        public List<string> KeyFields { get; set; } = new List<string>();
        public string JoinRelationship { get; set; }
    }

    public class FileOperation
    {
        public string FileName { get; set; }
        public string OperationType { get; set; } // READ, WRITE
        public string RecordFormat { get; set; }
        public int RecordLength { get; set; }
        public string Description { get; set; }
    }
}