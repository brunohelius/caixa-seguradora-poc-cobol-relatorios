using System;

namespace CaixaSeguradora.Core.Entities
{
    /// <summary>
    /// Base class providing audit trail fields for all entities.
    /// Automatically tracks creation and modification metadata for all database records.
    /// Supports compliance and troubleshooting requirements.
    /// </summary>
    public abstract class AuditableEntity
    {
        /// <summary>
        /// UTC timestamp when the record was created.
        /// Automatically set on first insert.
        /// </summary>
        public DateTime CreatedAt { get; set; } = DateTime.UtcNow;

        /// <summary>
        /// UTC timestamp when the record was last updated.
        /// Automatically set on every update operation.
        /// Null if record has never been updated.
        /// </summary>
        public DateTime? UpdatedAt { get; set; }

        /// <summary>
        /// User or system identifier who created the record.
        /// Examples: "system", "batch-job", "user@domain.com", "API-Client-123"
        /// </summary>
        public string CreatedBy { get; set; }

        /// <summary>
        /// User or system identifier who last updated the record.
        /// Null if record has never been updated.
        /// </summary>
        public string UpdatedBy { get; set; }

        /// <summary>
        /// Marks the entity as modified and updates the UpdatedAt timestamp.
        /// Call this method before saving changes to track modification time.
        /// </summary>
        /// <param name="modifiedBy">Identifier of the user/system making the modification</param>
        public virtual void MarkAsModified(string modifiedBy)
        {
            UpdatedAt = DateTime.UtcNow;
            UpdatedBy = modifiedBy;
        }

        /// <summary>
        /// Sets the creation metadata for the entity.
        /// Typically called by the repository layer before initial insert.
        /// </summary>
        /// <param name="createdBy">Identifier of the user/system creating the record</param>
        public virtual void SetCreationMetadata(string createdBy)
        {
            CreatedAt = DateTime.UtcNow;
            CreatedBy = createdBy;
        }
    }
}
