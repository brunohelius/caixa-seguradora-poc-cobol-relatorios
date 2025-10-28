#!/usr/bin/env python3
"""
Gerador de dataset CSV para testes de comparação COBOL vs .NET
Cria 1000+ registros com cenários variados para validação regulatória SUSEP
"""

import csv
import random
from datetime import datetime, timedelta
from decimal import Decimal

def generate_dataset(output_file: str, num_records: int = 1200):
    """Gera dataset CSV com múltiplos cenários de teste"""

    # Configurações
    companies = [10, 11]  # Caixa Seguradora companies
    ramos = [531, 541, 167, 1061, 860, 870, 993, 1641, 1648]  # SUSEP product codes
    movement_types = ['1', '2', '3', '4', '5', '6']  # 101-106
    states = ['SP', 'RJ', 'MG', 'RS', 'BA', 'PR', 'SC', 'PE', 'AM', 'GO', 'DF', 'ES', 'CE', 'PA']

    # Data base: Outubro 2025
    base_date = datetime(2025, 10, 1)

    records = []

    # Header
    header = [
        'PremiumId', 'PolicyNumber', 'EndorsementNumberCA', 'EndorsementNumber',
        'ProductCode', 'RamoSusep', 'MovementType', 'CompanyCode', 'IssueDate',
        'EffectiveDate', 'ExpirationDate', 'ProposalDate', 'ClientCode',
        'EstipulanteCode', 'ProducerCode', 'AgencyCode', 'StateCode',
        'PremiumAmountItem', 'IOFAmountItem', 'AdditionalAmountItem',
        'TotalAmountItem', 'TotalPremiumAmount', 'TotalIOFAmount',
        'TotalAdditionalAmount', 'GrandTotalAmount', 'CedidoParticipation',
        'CedidoPremium', 'CedidoCommission', 'ObtidoParticipation',
        'ObtidoPremium', 'ObtidoCommission', 'BilheteNumber', 'InsuredQuantity',
        'SusepProcessNumber', 'CoverageCount', 'CreatedAt', 'UpdatedAt'
    ]
    records.append(header)

    for i in range(1, num_records + 1):
        # Identificação
        premium_id = i
        policy_number = 1000000 + i
        endorsement_ca = random.randint(0, 99999) if random.random() > 0.7 else 0
        endorsement_num = random.randint(0, 99999) if random.random() > 0.8 else 0

        # Produto
        product_code = random.randint(1, 999)
        ramo_susep = random.choice(ramos)
        movement_type = random.choice(movement_types)
        company_code = random.choice(companies)

        # Datas
        issue_date = base_date + timedelta(days=random.randint(0, 30))
        effective_date = issue_date + timedelta(days=random.randint(0, 15))
        expiration_date = effective_date + timedelta(days=365)
        proposal_date = issue_date - timedelta(days=random.randint(1, 30))

        # Partes
        client_code = random.randint(100000, 999999)
        estipulante_code = random.randint(100000, 999999) if random.random() > 0.5 else client_code
        producer_code = random.randint(10000, 99999)
        agency_code = random.randint(1000, 9999)
        state_code = random.choice(states)

        # Valores financeiros (variados para testar banker's rounding)
        base_premium = Decimal(str(random.uniform(100.0, 50000.0)))

        # Cenários especiais de arredondamento bancário
        if i % 100 == 0:  # A cada 100 registros, teste .125 (banker's rounding)
            premium_item = Decimal('1234.125')
        elif i % 150 == 0:  # Teste .225
            premium_item = Decimal('5678.225')
        elif i % 200 == 0:  # Teste .625
            premium_item = Decimal('9012.625')
        else:
            premium_item = base_premium.quantize(Decimal('0.01'))

        iof_item = (premium_item * Decimal('0.0738')).quantize(Decimal('0.01'))
        additional_item = Decimal(str(random.uniform(0.0, 100.0))).quantize(Decimal('0.01'))
        total_item = premium_item + iof_item + additional_item

        # Totais (podem ter múltiplas coberturas)
        coverage_multiplier = random.randint(1, 5)
        total_premium = (premium_item * coverage_multiplier).quantize(Decimal('0.01'))
        total_iof = (iof_item * coverage_multiplier).quantize(Decimal('0.01'))
        total_additional = (additional_item * coverage_multiplier).quantize(Decimal('0.01'))
        grand_total = total_premium + total_iof + total_additional

        # Cosseguro (20% dos registros têm cosseguro)
        has_cosseguro = random.random() > 0.8

        if has_cosseguro:
            cedido_participation = Decimal(str(random.uniform(10.0, 50.0))).quantize(Decimal('0.01'))
            cedido_premium = (total_premium * cedido_participation / 100).quantize(Decimal('0.01'))
            cedido_commission = (cedido_premium * Decimal('0.15')).quantize(Decimal('0.01'))

            obtido_participation = Decimal(str(random.uniform(10.0, 30.0))).quantize(Decimal('0.01'))
            obtido_premium = (total_premium * obtido_participation / 100).quantize(Decimal('0.01'))
            obtido_commission = (obtido_premium * Decimal('0.10')).quantize(Decimal('0.01'))
        else:
            cedido_participation = Decimal('0.00')
            cedido_premium = Decimal('0.00')
            cedido_commission = Decimal('0.00')
            obtido_participation = Decimal('0.00')
            obtido_premium = Decimal('0.00')
            obtido_commission = Decimal('0.00')

        # Outros campos
        bilhete_number = random.randint(100000000, 999999999) if ramo_susep in [167, 1061] else 0
        insured_quantity = random.randint(1, 100) if ramo_susep == 1061 else 1
        susep_process = f"SUSEP{random.randint(10000000, 99999999):010d}{random.randint(100000, 999999):08d}"
        coverage_count = coverage_multiplier

        # Timestamps
        created_at = issue_date.strftime('%Y-%m-%d %H:%M:%S')
        updated_at = created_at

        # Record
        record = [
            premium_id, policy_number, endorsement_ca, endorsement_num,
            product_code, ramo_susep, movement_type, company_code,
            issue_date.strftime('%Y%m%d'), effective_date.strftime('%Y%m%d'),
            expiration_date.strftime('%Y%m%d'), proposal_date.strftime('%Y%m%d'),
            client_code, estipulante_code, producer_code, agency_code, state_code,
            f"{premium_item:.2f}", f"{iof_item:.2f}", f"{additional_item:.2f}",
            f"{total_item:.2f}", f"{total_premium:.2f}", f"{total_iof:.2f}",
            f"{total_additional:.2f}", f"{grand_total:.2f}",
            f"{cedido_participation:.2f}", f"{cedido_premium:.2f}", f"{cedido_commission:.2f}",
            f"{obtido_participation:.2f}", f"{obtido_premium:.2f}", f"{obtido_commission:.2f}",
            bilhete_number, insured_quantity, susep_process, coverage_count,
            created_at, updated_at
        ]
        records.append(record)

    # Escrever CSV
    with open(output_file, 'w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerows(records)

    print(f"✅ Dataset gerado: {output_file}")
    print(f"   Total de registros: {num_records}")
    print(f"   Registros com cosseguro: ~{int(num_records * 0.2)}")
    print(f"   Cenários de banker's rounding: {num_records // 100 + num_records // 150 + num_records // 200}")
    print(f"   Datas: {base_date.strftime('%Y-%m-%d')} até {(base_date + timedelta(days=30)).strftime('%Y-%m-%d')}")

if __name__ == "__main__":
    output_file = "golden-premiums.csv"
    generate_dataset(output_file, num_records=1200)
