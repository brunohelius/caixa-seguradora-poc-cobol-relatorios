# Mock Data Test Results

## Working CSV Uploads ✅

Successfully tested the following entity types:

### Products
- File: `products.csv`
- Records loaded: 3
- Status: ✅ SUCCESS
- Notes: Simple entity with no navigation properties

### Clients  
- File: `clients.csv`
- Records loaded: 5
- Status: ✅ SUCCESS
- Notes: Simple entity with no navigation properties

## Known Limitations ⚠️

### Entities with Foreign Key Navigation Properties

The following entities **cannot be loaded via CSV** due to EF Core change tracking conflicts:

- **Policies**: Has navigation properties to Client, Product, Agency, Producer
- **PremiumRecords**: Has navigation properties to Client, Product, Policy

**Root Cause**: When EF Core adds entities with foreign key values that reference existing records, it attempts to:
1. Load and track the related entities from the database
2. Attach those entities to maintain referential integrity
3. Multiple rows referencing the same foreign key cause "entity already tracked" errors

**Error Example**:
```
The instance of entity type 'Product' cannot be tracked because another instance 
with the key value '{ProductCode: 1001}' is already being tracked.
```

### Attempted Fixes

Tried the following approaches without success:
1. `ChangeTracker.AutoDetectChangesEnabled = false` - Still tracks navigation properties
2. `ChangeTracker.LazyLoadingEnabled = false` - Doesn't prevent eager fix-up
3. `ChangeTracker.Clear()` between batches - Error occurs during AddRangeAsync before SaveChanges

### Recommended Solution

For production use, implement one of:

1. **Raw SQL Bulk Insert**: Bypass EF Core entirely for CSV loads
   ```csharp
   await _context.Database.ExecuteSqlRawAsync(
       "INSERT INTO Policies (PolicyNumber, ClientCode, ProductCode, ...) VALUES (...)"
   );
   ```

2. **Disable Navigation Properties During Import**: Use DTOs without navigation properties for CSV import, then map to entities

3. **Use EF Core Bulk Extensions**: Libraries like EFCore.BulkExtensions that handle this scenario

4. **Load in Dependency Order**: Ensure all foreign key targets exist before loading dependent entities, and clear change tracker between entity types

## API Endpoint Test Status

| Endpoint | Method | Status | Notes |
|----------|--------|--------|-------|
| `/api/mock-data/load` | POST | ⚠️ Partial | Works for simple entities only |
| `/api/mock-data/stats` | GET | ✅ Tested | Returns record counts correctly |
| `/api/mock-data/validate` | GET | 🔄 Pending | Not yet tested |
| `/api/mock-data/clear/{entityType}` | DELETE | 🔄 Pending | Not yet tested |
| `/api/mock-data/reset` | POST | 🔄 Pending | Not yet tested |
| `/api/mock-data/schema` | GET | 🔄 Pending | Not yet tested |

## Test Summary

- **Total entities attempted**: 4 (Products, Clients, Policies, Premiums)
- **Successful loads**: 2 (Products, Clients)
- **Failed loads**: 2 (Policies, Premiums - navigation property conflicts)
- **Total records loaded**: 8 records (3 products + 5 clients)

## Next Steps

1. Test remaining API endpoints (validate, clear, reset, schema)
2. Implement frontend components for User Story 5
3. Document this limitation in technical documentation
4. Consider implementing raw SQL bulk insert for complex entities in Phase 2
