namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Database dependencies analysis showing tables, views, and relationships.
/// Critical for understanding data flow and migration impact.
/// </summary>
public class DatabaseDependenciesDto
{
    /// <summary>
    /// List of database tables/views accessed by the program
    /// </summary>
    public List<DatabaseTableDto> Tables { get; set; } = new();

    /// <summary>
    /// List of cursor declarations for batch processing
    /// </summary>
    public List<CursorDeclarationDto> Cursors { get; set; } = new();

    /// <summary>
    /// SQL operation statistics
    /// </summary>
    public SqlOperationStatsDto SqlStats { get; set; } = new();

    /// <summary>
    /// Data flow diagram (relationships between tables)
    /// </summary>
    public List<TableRelationshipDto> Relationships { get; set; } = new();

    /// <summary>
    /// Total number of tables/views
    /// </summary>
    public int TotalTables => Tables.Count;

    /// <summary>
    /// Total number of cursors
    /// </summary>
    public int TotalCursors => Cursors.Count;
}

/// <summary>
/// Database table or view information
/// </summary>
public class DatabaseTableDto
{
    /// <summary>
    /// Table/view name (e.g., V0PREMIOS, V0APOLICE)
    /// </summary>
    public string Name { get; set; } = string.Empty;

    /// <summary>
    /// Table type (TABLE, VIEW)
    /// </summary>
    public string Type { get; set; } = "VIEW";

    /// <summary>
    /// Description of the table purpose
    /// </summary>
    public string Description { get; set; } = string.Empty;

    /// <summary>
    /// Primary entity mapped to this table (C# class name)
    /// </summary>
    public string EntityName { get; set; } = string.Empty;

    /// <summary>
    /// Number of columns in table
    /// </summary>
    public int ColumnCount { get; set; }

    /// <summary>
    /// Access type (SELECT, INSERT, UPDATE, DELETE)
    /// </summary>
    public List<string> AccessTypes { get; set; } = new();

    /// <summary>
    /// Access frequency estimate (High, Medium, Low)
    /// </summary>
    public string AccessFrequency { get; set; } = "High";

    /// <summary>
    /// COBOL sections that access this table
    /// </summary>
    public List<string> AccessedBySections { get; set; } = new();

    /// <summary>
    /// Whether cursor processing is used
    /// </summary>
    public bool UsesCursor { get; set; }

    /// <summary>
    /// Estimated row count (for performance planning)
    /// </summary>
    public long EstimatedRowCount { get; set; }
}

/// <summary>
/// COBOL cursor declaration for batch processing
/// </summary>
public class CursorDeclarationDto
{
    /// <summary>
    /// Cursor name (e.g., CUR-V0PREMIOS)
    /// </summary>
    public string CursorName { get; set; } = string.Empty;

    /// <summary>
    /// Target table/view
    /// </summary>
    public string TargetTable { get; set; } = string.Empty;

    /// <summary>
    /// COBOL section where cursor is declared
    /// </summary>
    public string DeclaredInSection { get; set; } = string.Empty;

    /// <summary>
    /// WHERE clause conditions (simplified)
    /// </summary>
    public string WhereClause { get; set; } = string.Empty;

    /// <summary>
    /// ORDER BY clause (if any)
    /// </summary>
    public string OrderBy { get; set; } = string.Empty;

    /// <summary>
    /// Cursor type (Forward-Only, Scroll)
    /// </summary>
    public string CursorType { get; set; } = "Forward-Only";

    /// <summary>
    /// .NET implementation approach (IAsyncEnumerable, pagination, etc.)
    /// </summary>
    public string DotNetImplementation { get; set; } = "IAsyncEnumerable<T>";

    /// <summary>
    /// Estimated records processed per execution
    /// </summary>
    public long EstimatedRecordsProcessed { get; set; }
}

/// <summary>
/// SQL operation statistics
/// </summary>
public class SqlOperationStatsDto
{
    /// <summary>
    /// Total number of SELECT statements
    /// </summary>
    public int SelectCount { get; set; }

    /// <summary>
    /// Total number of INSERT statements
    /// </summary>
    public int InsertCount { get; set; }

    /// <summary>
    /// Total number of UPDATE statements
    /// </summary>
    public int UpdateCount { get; set; }

    /// <summary>
    /// Total number of DELETE statements
    /// </summary>
    public int DeleteCount { get; set; }

    /// <summary>
    /// Total SQL operations
    /// </summary>
    public int TotalOperations => SelectCount + InsertCount + UpdateCount + DeleteCount;

    /// <summary>
    /// Read-only percentage (SELECT only vs. DML)
    /// </summary>
    public decimal ReadOnlyPercentage => TotalOperations > 0
        ? (decimal)SelectCount / TotalOperations * 100
        : 100m;
}

/// <summary>
/// Relationship between database tables
/// </summary>
public class TableRelationshipDto
{
    /// <summary>
    /// Source table name
    /// </summary>
    public string SourceTable { get; set; } = string.Empty;

    /// <summary>
    /// Target table name
    /// </summary>
    public string TargetTable { get; set; } = string.Empty;

    /// <summary>
    /// Relationship type (One-to-One, One-to-Many, Many-to-Many)
    /// </summary>
    public string RelationshipType { get; set; } = "One-to-Many";

    /// <summary>
    /// Foreign key column(s)
    /// </summary>
    public List<string> ForeignKeyColumns { get; set; } = new();

    /// <summary>
    /// Relationship description
    /// </summary>
    public string Description { get; set; } = string.Empty;
}
