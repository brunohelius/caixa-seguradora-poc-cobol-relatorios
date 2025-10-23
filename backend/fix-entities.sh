#!/bin/bash

# This script adds missing properties to all entities to fix compilation errors

echo "Fixing Client.cs..."
# Add missing properties to Client
sed -i '' '/public ICollection<Address> Addresses/i\
\
    // Additional properties for repository compatibility\
    public int Id => ClientCode;  // Alias\
    public string Name => ClientName;  // Alias\
    public string PersonType => ClientType;  // Alias\
    public string TaxId => DocumentNumber;  // Alias\
    public ICollection<Policy> Policies { get; set; } = new List<Policy>();
' src/CaixaSeguradora.Core/Entities/Client.cs

echo "Fixing Product.cs..."
# Add missing properties to Product
sed -i '' '/public ICollection<Policy> Policies/i\
\
    // Additional properties for repository compatibility\
    public int Id => ProductCode;  // Alias\
    public string Description => ProductName;  // Alias\
    public string Status => ProductStatus;  // Alias\
    public decimal CommissionPercentage { get; set; }\
    public string ProductDescription => ProductName;  // Alias
' src/CaixaSeguradora.Core/Entities/Product.cs

echo "Fixing Endorsement.cs..."
# Add missing properties to Endorsement
sed -i '' '/public Policy Policy/i\
\
    // Additional properties for repository compatibility\
    public long PolicyNumber { get; set; }\
    public string CancellationFlag { get; set; } = "N";
' src/CaixaSeguradora.Core/Entities/Endorsement.cs

echo "Fixing Coverage.cs..."
# Add missing properties to Coverage
sed -i '' '/public Policy Policy/i\
\
    // Additional properties for repository compatibility\
    public long CoverageId => Id;  // Alias\
    public string CoverageType { get; set; } = string.Empty;\
    public long PolicyNumber { get; set; }\
    public ICollection<Coverage> Coverages { get; set; } = new List<Coverage>();
' src/CaixaSeguradora.Core/Entities/Coverage.cs

echo "Fixing Address.cs..."
# Add missing properties to Address
sed -i '' '/public Client Client/i\
\
    // Additional properties for repository compatibility\
    public long Id => AddressId;  // Alias\
    public string Street => StreetAddress;  // Alias\
    public string Country { get; set; } = "Brazil";\
    public int ClientId => ClientCode;  // Alias
' src/CaixaSeguradora.Core/Entities/Address.cs

echo "All entities fixed!"
