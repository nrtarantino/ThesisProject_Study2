#!/usr/bin/env python3
"""Combine study2_*.csv files and add observed_slope, math_level from manifests/Qualtrics."""

import csv
from pathlib import Path

# Qualtrics columns: I = ResponseId (match key), V = math_level
QUALTRICS_ID_COL = 8   # 0-based index for column I
QUALTRICS_MATH_COL = 21  # 0-based index for column V


def load_manifest_by_index(filepath):
    """Load manifest and return dict mapping index -> observed_slope"""
    mapping = {}
    with open(filepath, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        for row in reader:
            idx = int(row['index'])
            mapping[idx] = row['observed_slope']
    return mapping


def load_qualtrics_math_levels(filepath):
    """Load Qualtrics CSV: map ResponseId (col I) -> math_level (col V)"""
    mapping = {}
    with open(filepath, 'r', encoding='utf-8') as f:
        reader = csv.reader(f)
        header = next(reader)
        for row in reader:
            if len(row) > max(QUALTRICS_ID_COL, QUALTRICS_MATH_COL):
                response_id = row[QUALTRICS_ID_COL].strip()
                math_level = row[QUALTRICS_MATH_COL].strip() if len(row) > QUALTRICS_MATH_COL else ''
                mapping[response_id] = math_level
    return mapping


def main():
    script_dir = Path(__file__).parent
    project_root = script_dir.parent
    main_manifest = project_root / 'stimuli' / 'manifest.csv'
    training_manifest = project_root / 'stimuli' / 'manifest_training.csv'

    # Step 1: Combine CSVs
    csv_files = sorted(p for p in script_dir.glob('study2_*.csv')
                      if p.name != 'study2_combined.csv')

    if not csv_files:
        print('No study2_*.csv files found in', script_dir)
        return

    print(f'Found {len(csv_files)} CSV files')

    all_rows = []
    headers = None

    for filepath in csv_files:
        with open(filepath, 'r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            if headers is None:
                headers = list(reader.fieldnames) + ['source_file']
            count = 0
            for row in reader:
                row['source_file'] = filepath.name
                all_rows.append(row)
                count += 1
        print(f'  {filepath.name}: {count} rows')

    output_path = script_dir / 'study2_combined.csv'
    headers = headers + ['observed_slope', 'math_level']

    # Step 2: Load manifests and add observed_slope
    main_slopes = load_manifest_by_index(main_manifest) if main_manifest.exists() else {}
    training_slopes = load_manifest_by_index(training_manifest) if training_manifest.exists() else {}
    print(f'\nLoaded main manifest: {len(main_slopes)} stimuli')
    print(f'Loaded training manifest: {len(training_slopes)} stimuli')

    # Step 3: Load Qualtrics data (col I = ResponseId, col V = math_level)
    qualtrics_path = script_dir / 'qualtricsdata.csv'
    math_levels = load_qualtrics_math_levels(qualtrics_path) if qualtrics_path.exists() else {}
    print(f'Loaded Qualtrics math_level: {len(math_levels)} participants')

    for row in all_rows:
        idx = int(row['stimulusIndex'])
        trial_type = row.get('trialType', 'main')
        if trial_type == 'training':
            row['observed_slope'] = training_slopes.get(idx, '')
        else:
            row['observed_slope'] = main_slopes.get(idx, '')
        row['math_level'] = math_levels.get(row.get('qualtricsResponseId', ''), '')

    with open(output_path, 'w', newline='', encoding='utf-8') as f:
        writer = csv.DictWriter(f, fieldnames=headers)
        writer.writeheader()
        writer.writerows(all_rows)

    print(f'\nCombined {len(all_rows)} rows with observed_slope and math_level into {output_path}')


if __name__ == '__main__':
    main()
