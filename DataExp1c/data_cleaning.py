#!/usr/bin/env python3
"""Combine study2c_*.csv files and add observed_slope and Qualtrics fields."""

import csv
from pathlib import Path

# Fallback if column names missing (legacy export): I = ResponseId, V = Q2
QUALTRICS_ID_COL = 8   # 0-based index for column I
QUALTRICS_MATH_COL = 21  # 0-based index for column V (Q2)

QUALTRICS_EXTRA_COLS = (
    'Q4_1_TEXT', 'Q4_2_TEXT', 'Q7', 'Q8', 'Q9', 'Q10', 'Q10_7_TEXT',
    'Q11', 'Q12', 'Q13',
)
QUALTRICS_OUTPUT_COLS = ('math_level',) + QUALTRICS_EXTRA_COLS


def _is_other_math_choice(q2: str) -> bool:
    """True when Q2 is the Other option (use Q2_7_TEXT instead)."""
    s = (q2 or '').strip().lower()
    if not s:
        return False
    return s == 'other' or s.startswith('other:')


def load_manifest_by_index(filepath):
    """Load manifest and return dict mapping index -> observed_slope"""
    mapping = {}
    with open(filepath, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        for row in reader:
            idx = int(row['index'])
            mapping[idx] = row['observed_slope']
    return mapping


def load_qualtrics_participant_data(filepath):
    """Load Qualtrics CSV: map ResponseId -> survey fields (math_level + QUALTRICS_EXTRA_COLS)."""
    mapping = {}
    with open(filepath, 'r', encoding='utf-8-sig') as f:
        reader = csv.reader(f)
        header = next(reader)
        try:
            resp_idx = header.index('ResponseId')
        except ValueError:
            resp_idx = QUALTRICS_ID_COL
        try:
            q2_idx = header.index('Q2')
        except ValueError:
            q2_idx = QUALTRICS_MATH_COL
        q2_text_idx = header.index('Q2_7_TEXT') if 'Q2_7_TEXT' in header else None
        extra_indices = {
            col: header.index(col) if col in header else None
            for col in QUALTRICS_EXTRA_COLS
        }

        for row in reader:
            if len(row) <= resp_idx:
                continue
            response_id = row[resp_idx].strip()
            if not response_id.startswith('R_'):
                continue
            q2 = row[q2_idx].strip() if q2_idx < len(row) else ''
            q2_text = row[q2_text_idx].strip() if q2_text_idx is not None and q2_text_idx < len(row) else ''
            if _is_other_math_choice(q2) and q2_text_idx is not None:
                math_level = q2_text
            else:
                math_level = q2
            data = {'math_level': math_level}
            for col, idx in extra_indices.items():
                data[col] = row[idx].strip() if idx is not None and idx < len(row) else ''
            mapping[response_id] = data
    return mapping


def main():
    script_dir = Path(__file__).parent
    project_root = script_dir.parent
    main_manifest = project_root / 'stimuli2' / 'manifest.csv'

    # Step 1: Combine CSVs
    csv_files = sorted(p for p in script_dir.glob('study2c_*.csv')
                      if p.name != 'study2c_combined.csv')

    if not csv_files:
        print('No study2c_*.csv files found in', script_dir)
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

    output_path = script_dir / 'study2c_combined.csv'
    headers = headers + ['observed_slope', *QUALTRICS_OUTPUT_COLS]

    # Step 2: Load main manifest (study2c uses main manifest for both training and main trials)
    main_slopes = load_manifest_by_index(main_manifest) if main_manifest.exists() else {}
    print(f'\nLoaded main manifest: {len(main_slopes)} stimuli')

    # Step 3: Load Qualtrics (Q2 math level; if Q2 is Other/Other:, use Q2_7_TEXT)
    qualtrics_path = script_dir / 'qualtricsdata.csv'
    qualtrics_data = load_qualtrics_participant_data(qualtrics_path) if qualtrics_path.exists() else {}
    print(f'Loaded Qualtrics fields for {len(qualtrics_data)} participants')

    for row in all_rows:
        idx = int(row['stimulusIndex'])
        row['observed_slope'] = main_slopes.get(idx, '')
        pdata = qualtrics_data.get(row.get('qualtricsResponseId', ''), {})
        for col in QUALTRICS_OUTPUT_COLS:
            row[col] = pdata.get(col, '')

    with open(output_path, 'w', newline='', encoding='utf-8') as f:
        writer = csv.DictWriter(f, fieldnames=headers)
        writer.writeheader()
        writer.writerows(all_rows)

    print(f'\nCombined {len(all_rows)} rows into {output_path}')


if __name__ == '__main__':
    main()
