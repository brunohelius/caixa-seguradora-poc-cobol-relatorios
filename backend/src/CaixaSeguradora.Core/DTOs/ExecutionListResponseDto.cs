namespace CaixaSeguradora.Core.DTOs
{
    /// <summary>
    /// Paginated response for execution history list.
    /// </summary>
    public class ExecutionListResponseDto
    {
        /// <summary>
        /// List of execution records for current page.
        /// </summary>
        public List<ReportExecutionDto> Items { get; set; } = new();

        /// <summary>
        /// Total number of executions matching filter criteria.
        /// </summary>
        public int TotalCount { get; set; }

        /// <summary>
        /// Current page number (1-based).
        /// </summary>
        public int Page { get; set; }

        /// <summary>
        /// Number of items per page.
        /// </summary>
        public int PageSize { get; set; }

        /// <summary>
        /// Total number of pages.
        /// </summary>
        public int TotalPages => PageSize > 0 ? (int)Math.Ceiling((double)TotalCount / PageSize) : 0;

        /// <summary>
        /// Whether there is a next page.
        /// </summary>
        public bool HasNextPage => Page < TotalPages;

        /// <summary>
        /// Whether there is a previous page.
        /// </summary>
        public bool HasPreviousPage => Page > 1;
    }
}
