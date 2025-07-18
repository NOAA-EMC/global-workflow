# Rocotostat.py Test Report

## Summary
- **Total Tests**: 18
- **Passed**: 0 ✅
- **Partial**: 12 ⚠️
- **Failed**: 6 ❌
- **Success Rate**: 0.0%

## Test Results

### base_workflow_default
**Status**: ❌ FAILED
**Scenario**: Base working workflow with all tasks succeeding
**Args**: None

### base_workflow_-v
**Status**: ❌ FAILED
**Scenario**: Base working workflow with all tasks succeeding
**Args**: ['-v']

### base_workflow_-t_stage_test
**Status**: ❌ FAILED
**Scenario**: Base working workflow with all tasks succeeding
**Args**: ['-t', 'stage_test']

### base_workflow_-c_202507181200
**Status**: ❌ FAILED
**Scenario**: Base working workflow with all tasks succeeding
**Args**: ['-c', '202507181200']

### base_workflow_-T
**Status**: ❌ FAILED
**Scenario**: Base working workflow with all tasks succeeding
**Args**: ['-T']

### base_workflow_-s
**Status**: ❌ FAILED
**Scenario**: Base working workflow with all tasks succeeding
**Args**: ['-s']

### failing_workflow_default
**Status**: ⚠️ PARTIAL
**Scenario**: Workflow with intentionally failing stage_test task
**Args**: None
**Differences**:
- Line 3: Official: `202507181200              stage_test                          65              QUEUED                   -         0           0.0` | Custom: `202507181200              stage_test                           -                   -                   -         -             -`
- Line 8: Official: `202507181800              stage_test                          66              QUEUED                   -         0           0.0` | Custom: `202507181800              stage_test                           -                   -                   -         -             -`

### failing_workflow_-v
**Status**: ⚠️ PARTIAL
**Scenario**: Workflow with intentionally failing stage_test task
**Args**: ['-v']
**Differences**:
- Line 3: Official: `202507181200              stage_test                          65              QUEUED                   -         0           0.0` | Custom: `202507181200              stage_test                           -                   -                   -         -             -`
- Line 8: Official: `202507181800              stage_test                          66              QUEUED                   -         0           0.0` | Custom: `202507181800              stage_test                           -                   -                   -         -             -`

### failing_workflow_-t_stage_test
**Status**: ⚠️ PARTIAL
**Scenario**: Workflow with intentionally failing stage_test task
**Args**: ['-t', 'stage_test']
**Differences**:
- Line 3: Official: `202507181200              stage_test                          65              QUEUED                   -         0           0.0` | Custom: `202507181200              stage_test                           -                   -                   -         -             -`
- Line 5: Official: `202507181800              stage_test                          66              QUEUED                   -         0           0.0` | Custom: `202507181800              stage_test                           -                   -                   -         -             -`

### failing_workflow_-c_202507181200
**Status**: ⚠️ PARTIAL
**Scenario**: Workflow with intentionally failing stage_test task
**Args**: ['-c', '202507181200']
**Differences**:
- Line 3: Official: `202507181200              stage_test                          65              QUEUED                   -         0           0.0` | Custom: `202507181200              stage_test                           -                   -                   -         -             -`

### failing_workflow_-T
**Status**: ⚠️ PARTIAL
**Scenario**: Workflow with intentionally failing stage_test task
**Args**: ['-T']
**Differences**:
- Line 3: Official: `          stage_test    202507181200                          65              QUEUED                   -         0           0.0` | Custom: `        cleanup_test    202507181200                          -               -                        -         -           -`
- Line 4: Official: `          stage_test    202507181800                          66              QUEUED                   -         0           0.0` | Custom: `        cleanup_test    202507181800                          -               -                        -         -           -`
- Line 6: Official: `            run_test    202507181200                           -                   -                   -         -             -` | Custom: `           post_test    202507181200                          -               -                        -         -           -`
- Line 7: Official: `            run_test    202507181800                           -                   -                   -         -             -` | Custom: `           post_test    202507181800                          -               -                        -         -           -`
- Line 9: Official: `           post_test    202507181200                           -                   -                   -         -             -` | Custom: `            run_test    202507181200                          -               -                        -         -           -`
- Line 10: Official: `           post_test    202507181800                           -                   -                   -         -             -` | Custom: `            run_test    202507181800                          -               -                        -         -           -`
- Line 12: Official: `        cleanup_test    202507181200                           -                   -                   -         -             -` | Custom: `          stage_test    202507181200                          -               -                        -         -           -`
- Line 13: Official: `        cleanup_test    202507181800                           -                   -                   -         -             -` | Custom: `          stage_test    202507181800                          -               -                        -         -           -`

### failing_workflow_-s
**Status**: ⚠️ PARTIAL
**Scenario**: Workflow with intentionally failing stage_test task
**Args**: ['-s']
**Differences**:
- Line 2: Official: `202507181200      Active    Jul 18 2025 22:18:31             -          ` | Custom: `202507181200      Active    Jul 18 2025 21:26:18             -         `
- Line 3: Official: `202507181800      Active    Jul 18 2025 22:18:31             -          ` | Custom: `202507181800      Active    Jul 18 2025 21:26:18             -         `

### multi_cycle_default
**Status**: ⚠️ PARTIAL
**Scenario**: Workflow with multiple cycles (every 30 minutes)
**Args**: None
**Differences**:
- Line 3: Official: `202507181200              stage_test                          67              QUEUED                   -         0           0.0` | Custom: `202507181200              stage_test                           -                   -                   -         -             -`
- Line 8: Official: `202507181800              stage_test                          68              QUEUED                   -         0           0.0` | Custom: `202507181800              stage_test                           -                   -                   -         -             -`

### multi_cycle_-v
**Status**: ⚠️ PARTIAL
**Scenario**: Workflow with multiple cycles (every 30 minutes)
**Args**: ['-v']
**Differences**:
- Line 3: Official: `202507181200              stage_test                          67              QUEUED                   -         0           0.0` | Custom: `202507181200              stage_test                           -                   -                   -         -             -`
- Line 8: Official: `202507181800              stage_test                          68              QUEUED                   -         0           0.0` | Custom: `202507181800              stage_test                           -                   -                   -         -             -`

### multi_cycle_-t_stage_test
**Status**: ⚠️ PARTIAL
**Scenario**: Workflow with multiple cycles (every 30 minutes)
**Args**: ['-t', 'stage_test']
**Differences**:
- Line 3: Official: `202507181200              stage_test                          67              QUEUED                   -         0           0.0` | Custom: `202507181200              stage_test                           -                   -                   -         -             -`
- Line 5: Official: `202507181800              stage_test                          68              QUEUED                   -         0           0.0` | Custom: `202507181800              stage_test                           -                   -                   -         -             -`

### multi_cycle_-c_202507181200
**Status**: ⚠️ PARTIAL
**Scenario**: Workflow with multiple cycles (every 30 minutes)
**Args**: ['-c', '202507181200']
**Differences**:
- Line 3: Official: `202507181200              stage_test                          67              QUEUED                   -         0           0.0` | Custom: `202507181200              stage_test                           -                   -                   -         -             -`

### multi_cycle_-T
**Status**: ⚠️ PARTIAL
**Scenario**: Workflow with multiple cycles (every 30 minutes)
**Args**: ['-T']
**Differences**:
- Line 3: Official: `          stage_test    202507181200                          67              QUEUED                   -         0           0.0` | Custom: `        cleanup_test    202507181200                          -               -                        -         -           -`
- Line 4: Official: `          stage_test    202507181800                          68              QUEUED                   -         0           0.0` | Custom: `        cleanup_test    202507181800                          -               -                        -         -           -`
- Line 6: Official: `            run_test    202507181200                           -                   -                   -         -             -` | Custom: `           post_test    202507181200                          -               -                        -         -           -`
- Line 7: Official: `            run_test    202507181800                           -                   -                   -         -             -` | Custom: `           post_test    202507181800                          -               -                        -         -           -`
- Line 9: Official: `           post_test    202507181200                           -                   -                   -         -             -` | Custom: `            run_test    202507181200                          -               -                        -         -           -`
- Line 10: Official: `           post_test    202507181800                           -                   -                   -         -             -` | Custom: `            run_test    202507181800                          -               -                        -         -           -`
- Line 12: Official: `        cleanup_test    202507181200                           -                   -                   -         -             -` | Custom: `          stage_test    202507181200                          -               -                        -         -           -`
- Line 13: Official: `        cleanup_test    202507181800                           -                   -                   -         -             -` | Custom: `          stage_test    202507181800                          -               -                        -         -           -`

### multi_cycle_-s
**Status**: ⚠️ PARTIAL
**Scenario**: Workflow with multiple cycles (every 30 minutes)
**Args**: ['-s']
**Differences**:
- Line 2: Official: `202507181200      Active    Jul 18 2025 22:18:31             -          ` | Custom: `202507181200      Active    Jul 18 2025 21:26:18             -         `
- Line 3: Official: `202507181800      Active    Jul 18 2025 22:18:31             -          ` | Custom: `202507181800      Active    Jul 18 2025 21:26:18             -         `

