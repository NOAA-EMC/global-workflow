#!/bin/bash
# 
# Rocoto Performance Analysis Script (Text-based)
# 
# This script provides quick text-based performance analysis using standard Linux tools
# when matplotlib/GUI visualization is not available.

LOG_DIR="/home/tmcguinness/GITHUB/COPILOT/ANALYS_LOGS/ci-global-workflows/ci/error_logs/ROCOTO_LOGS_5"
OUTPUT_DIR="/home/tmcguinness/GITHUB/COPILOT/global-workflow_forked"

echo "=== ROCOTO WORKFLOW PERFORMANCE ANALYSIS ==="
echo "Date: $(date)"
echo "Log Directory: ${LOG_DIR}"
echo

# Function to extract metrics from logs
extract_metrics() {
    local log_file="$1"
    local workflow_name=$(basename "${log_file}" _rocotostat.log)
    
    #echo "Analyzing: ${workflow_name}"
    
    # Count total sessions
    local total_sessions=$(grep -c "\[START\]" "${log_file}")
    
    # Extract call times
    grep "call_time=" "${log_file}" | sed 's/.*call_time=\([0-9.]*\)s.*/\1/' > "/tmp/${workflow_name}_call_times.txt"
    
    # Calculate average call time
    local avg_call_time=$(awk '{sum+=$1; count++} END {if(count>0) print sum/count; else print 0}' "/tmp/${workflow_name}_call_times.txt")
    
    # Extract thread counts
    grep "\[START\]" "${log_file}" | sed 's/.*has \([0-9]*\)\/[0-9]* threads.*/\1/' > "/tmp/${workflow_name}_start_threads.txt"
    grep "\[END\]" "${log_file}" | sed 's/.*has \([0-9]*\)\/[0-9]* threads.*/\1/' > "/tmp/${workflow_name}_end_threads.txt"
    
    # Calculate average thread usage
    local avg_start_threads=$(awk '{sum+=$1; count++} END {if(count>0) print sum/count; else print 0}' "/tmp/${workflow_name}_start_threads.txt")
    local avg_end_threads=$(awk '{sum+=$1; count++} END {if(count>0) print sum/count; else print 0}' "/tmp/${workflow_name}_end_threads.txt")
    
    # Count failures
    local failed_attempts=$(grep -c "ROCOTO_FAILED_ATTEMPT" "${log_file}")
    local success_attempts=$(grep -c "ROCOTO_SUCCESS_ATTEMPT" "${log_file}")
    
    # Calculate success rate
    local total_attempts=$((failed_attempts + success_attempts))
    local success_rate=0
    if [ "${total_attempts}" -gt 0 ]; then
        success_rate=$(echo "scale=3; ${success_attempts}/${total_attempts}" | bc -l)
    fi
    
    # Output results
    printf "%-25s %8d %8.3f %8.1f %8.1f %8.3f %8d %8d\n" \
        "${workflow_name}" \
        "${total_sessions}" \
        "${avg_call_time}" \
        "${avg_start_threads}" \
        "${avg_end_threads}" \
        "${success_rate}" \
        "${success_attempts}" \
        "${failed_attempts}"
}

# Create summary report
{
    echo "=== WORKFLOW PERFORMANCE SUMMARY ==="
    echo
    printf "%-25s %8s %8s %8s %8s %8s %8s %8s\n" \
        "Workflow" "Sessions" "AvgCall" "StartThr" "EndThr" "Success" "Success#" "Failed#"
    printf "%-25s %8s %8s %8s %8s %8s %8s %8s\n" \
        "-------------------------" "--------" "--------" "--------" "--------" "--------" "--------" "--------"
    
    for log_file in "${LOG_DIR}"/*.log; do
        if [ -f "${log_file}" ]; then
            extract_metrics "${log_file}"
        fi
    done
} > "${OUTPUT_DIR}/rocoto_text_summary.txt"

# Display the summary
cat "${OUTPUT_DIR}/rocoto_text_summary.txt"

echo
echo "=== CALL TIME DISTRIBUTION ==="

# Create a simple histogram using gnuplot if available, otherwise use awk
if command -v gnuplot >/dev/null 2>&1; then
    echo "Creating call time histogram with gnuplot..."
    
    # Combine all call times
    cat /tmp/*_call_times.txt > /tmp/all_call_times.txt
    
    # Create gnuplot script
    cat > /tmp/histogram.gnuplot << 'EOF'
set terminal png size 800,600
set output '/home/tmcguinness/GITHUB/COPILOT/global-workflow_forked/call_time_histogram.png'
set title "Rocoto Call Time Distribution"
set xlabel "Call Time (seconds)"
set ylabel "Frequency"
set grid
set style fill solid 0.5
bin_width = 0.05
bin_number(x) = floor(x/bin_width)
rounded(x) = bin_width * ( bin_number(x) + 0.5 )
plot '/tmp/all_call_times.txt' using (rounded($1)):(1) smooth frequency with boxes title "Call Times"
EOF
    
    gnuplot /tmp/histogram.gnuplot
    echo "Histogram saved to: ${OUTPUT_DIR}/call_time_histogram.png"
else
    echo "Creating text-based histogram..."
    # Simple text histogram
    awk '
    {
        bucket = int($1 * 10) / 10
        count[bucket]++
        if (bucket > max) max = bucket
        if (bucket < min || min == 0) min = bucket
    }
    END {
        print "Call Time Distribution (0.1s buckets):"
        for (i = min; i <= max; i += 0.1) {
            printf "%.1f-%.1f: ", i, i+0.1
            for (j = 0; j < count[i]; j++) printf "*"
            printf " (%d)\n", count[i]
        }
    }' /tmp/all_call_times.txt
fi

echo
echo "=== THREAD UTILIZATION ANALYSIS ==="

# Analyze thread utilization patterns
{
    echo "Thread Utilization Patterns:"
    echo
    for log_file in "${LOG_DIR}"/*.log; do
        if [ -f "${log_file}" ]; then
            workflow_name=$(basename "${log_file}" _rocotostat.log)
            echo "=== ${workflow_name} ==="
            
            # Extract utilization percentages
            grep "threads.*utilization" "${log_file}" | \
            sed 's/.*(\([0-9.]*\)% utilization).*/\1/' | \
            awk '{
                sum += $1
                count++
                if ($1 > max) max = $1
                if ($1 < min || min == 0) min = $1
            }
            END {
                if (count > 0) {
                    printf "  Sessions: %d\n", count
                    printf "  Avg Utilization: %.3f%%\n", sum/count
                    printf "  Min Utilization: %.3f%%\n", min
                    printf "  Max Utilization: %.3f%%\n", max
                }
            }'
            echo
        fi
    done
} >> "${OUTPUT_DIR}/rocoto_text_summary.txt"

echo
echo "=== PERFORMANCE RANKINGS ==="

# Sort workflows by performance metrics
{
    echo "=== TOP PERFORMERS (by average call time) ==="
    tail -n +4 "${OUTPUT_DIR}/rocoto_text_summary.txt" | head -n -20 | sort -k3,3n | head -3
    echo
    echo "=== MOST RELIABLE (by success rate) ==="
    tail -n +4 "${OUTPUT_DIR}/rocoto_text_summary.txt" | head -n -20 | sort -k6,6nr | head -3
    echo
    echo "=== HIGHEST THREAD USAGE (by start threads) ==="
    tail -n +4 "${OUTPUT_DIR}/rocoto_text_summary.txt" | head -n -20 | sort -k4,4nr | head -3
}

# Create a simple CSV for further analysis
{
    echo "workflow,sessions,avg_call_time,avg_start_threads,avg_end_threads,success_rate,success_count,failed_count"
    for log_file in "${LOG_DIR}"/*.log; do
        if [ -f "${log_file}" ]; then
            workflow_name=$(basename "${log_file}" _rocotostat.log)
            
            total_sessions=$(grep -c "\[START\]" "${log_file}")
            
            grep "call_time=" "${log_file}" | sed 's/.*call_time=\([0-9.]*\)s.*/\1/' > "/tmp/${workflow_name}_call_times.txt"
            avg_call_time=$(awk '{sum+=$1; count++} END {if(count>0) print sum/count; else print 0}' "/tmp/${workflow_name}_call_times.txt")
            
            grep "\[START\]" "${log_file}" | sed 's/.*has \([0-9]*\)\/[0-9]* threads.*/\1/' > "/tmp/${workflow_name}_start_threads.txt"
            grep "\[END\]" "${log_file}" | sed 's/.*has \([0-9]*\)\/[0-9]* threads.*/\1/' > "/tmp/${workflow_name}_end_threads.txt"
            
            avg_start_threads=$(awk '{sum+=$1; count++} END {if(count>0) print sum/count; else print 0}' "/tmp/${workflow_name}_start_threads.txt")
            avg_end_threads=$(awk '{sum+=$1; count++} END {if(count>0) print sum/count; else print 0}' "/tmp/${workflow_name}_end_threads.txt")
            
            failed_attempts=$(grep -c "ROCOTO_FAILED_ATTEMPT" "${log_file}")
            success_attempts=$(grep -c "ROCOTO_SUCCESS_ATTEMPT" "${log_file}")
            total_attempts=$((failed_attempts + success_attempts))
            
            if [ "${total_attempts}" -gt 0 ]; then
                success_rate=$(echo "scale=3; ${success_attempts}/${total_attempts}" | bc -l)
            else
                success_rate=0
            fi
            
            echo "${workflow_name},${total_sessions},${avg_call_time},${avg_start_threads},${avg_end_threads},${success_rate},${success_attempts},${failed_attempts}"
        fi
    done
} > "${OUTPUT_DIR}/rocoto_performance_data.csv"

# Clean up temporary files
rm -f /tmp/*_call_times.txt /tmp/*_start_threads.txt /tmp/*_end_threads.txt /tmp/all_call_times.txt /tmp/histogram.gnuplot

echo
echo "=== ANALYSIS COMPLETE ==="
echo "Text summary: ${OUTPUT_DIR}/rocoto_text_summary.txt"
echo "CSV data: ${OUTPUT_DIR}/rocoto_performance_data.csv"
echo "Python charts: ${OUTPUT_DIR}/rocoto_performance_analysis.png"
echo "Correlation heatmap: ${OUTPUT_DIR}/rocoto_correlation_heatmap.png"
echo "Summary statistics: ${OUTPUT_DIR}/rocoto_performance_summary.csv"
