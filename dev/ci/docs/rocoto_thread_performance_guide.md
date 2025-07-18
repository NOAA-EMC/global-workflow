# Rocoto Thread Usage and Performance Recommendations for HPC Environments

## Overview

This document provides an analysis of Rocoto's thread usage patterns and recommendations for optimizing performance in High Performance Computing (HPC) environments, particularly when using the `rocotostat.py` utility.

## Understanding Rocoto's Thread Model

### Thread Creation Pattern
Rocoto uses a thread-based architecture where each call to rocoto utilities (like `rocotostat`) creates threads that handle asynchronous communication between:
- The HPC platform scheduler (e.g., SLURM, PBS, SGE)
- The SQL database storing workflow state information

### Thread-Database Communication
- Each `rocotostat` call spawns threads that query the workflow database
- These threads maintain connection pools to the database
- Multiple simultaneous calls can lead to connection pool exhaustion
- Database locks can occur when many threads try to access the same workflow state

## Performance Issues in HPC Environments

### 1. Thread Pool Exhaustion
When multiple CI/CD processes run simultaneously:
- Each process spawns multiple `rocotostat` calls
- Each call creates its own thread pool
- System resources become over-allocated
- Response times degrade exponentially

### 2. Database Contention
- Multiple threads competing for database locks
- Increased I/O wait times
- Potential deadlocks in high-concurrency scenarios

### 3. Network Overhead
- Each thread maintains network connections to database
- Network bandwidth can become saturated
- Latency increases with concurrent connections

## Implemented Optimizations

### 1. Telescoping Delay Mechanism
The enhanced `attempt_multiple_times()` function implements exponential backoff:
- **Base delay**: 30 seconds (reduced from 120 seconds)
- **Telescoping pattern**: delay *= 2^(attempt-1)
- **Example**: 30s, 60s, 120s, 240s...

Benefits:
- Reduces system load during peak usage
- Allows congested systems time to recover
- Prevents thundering herd problems

### 2. Performance Monitoring
Added comprehensive logging to track:
- Individual call response times
- Total attempt duration
- Failure patterns and causes
- System performance degradation indicators

### 3. Reduced Call Frequency
- Lowered base delay from 120s to 30s for faster initial response
- Telescoping delays prevent rapid successive calls
- Smarter retry logic reduces unnecessary attempts

## Performance Recommendations

### 1. Database Optimization
```bash
# Increase database connection pool size
export ROCOTO_DB_POOL_SIZE=20

# Enable connection pooling
export ROCOTO_DB_POOL_ENABLED=true

# Set reasonable timeout values
export ROCOTO_DB_TIMEOUT=60
```

### 2. Concurrent Call Management
```bash
# Limit concurrent rocotostat processes
export MAX_CONCURRENT_ROCOTO_CALLS=5

# Use process semaphores in CI/CD scripts
sem --jobs 5 rocotostat -w workflow.xml -d database.db
```

### 3. Resource Monitoring
```bash
# Monitor database performance
iostat -x 1 | grep rocoto_db

# Track thread usage
ps -eLf | grep rocotostat | wc -l

# Monitor connection counts
netstat -an | grep :5432 | grep ESTABLISHED | wc -l
```

### 4. CI/CD Pipeline Optimization
- Stagger workflow checks across time intervals
- Implement circuit breaker patterns for failed checks
- Use shared caching for frequently accessed workflow states
- Batch multiple checks into single database queries where possible

### 5. System Configuration
```bash
# Increase system thread limits
echo "* soft nproc 65536" >> /etc/security/limits.conf
echo "* hard nproc 65536" >> /etc/security/limits.conf

# Optimize database connections
echo "max_connections = 200" >> postgresql.conf
echo "shared_buffers = 256MB" >> postgresql.conf
```

## Monitoring and Alerting

### Key Metrics to Track
1. **Response Time**: Average rocotostat call duration
2. **Thread Count**: Number of active rocoto threads
3. **Database Connections**: Active database connections
4. **Failure Rate**: Percentage of failed rocotostat calls
5. **Queue Depth**: Number of pending workflow checks

### Alerting Thresholds
- Response time > 300 seconds
- Thread count > 50 per node
- Database connections > 80% of pool
- Failure rate > 10%

## Implementation Notes

### Telescoping Delay Formula
```python
current_delay = base_delay * (2 ** (attempt - 1))
```

### Performance Logging Format
```
INFO: Rocoto call successful on attempt 1: call_time=2.34s, total_time=2.34s
WARNING: Rocoto call failed on attempt 2: call_time=15.67s, error=Connection timeout
INFO: Waiting 60s before retry attempt 3
```

## Future Enhancements

### 1. Connection Pooling
Implement shared connection pools across multiple rocotostat processes:
```python
from sqlalchemy import create_engine
from sqlalchemy.pool import StaticPool

engine = create_engine(
    database_url,
    poolclass=StaticPool,
    pool_size=20,
    max_overflow=0
)
```

### 2. Caching Layer
Add Redis or Memcached for frequently accessed workflow states:
```python
import redis
cache = redis.Redis(host='localhost', port=6379, db=0)
```

### 3. Asynchronous Processing
Implement async/await patterns for non-blocking database operations:
```python
import asyncio
import aiopg

async def get_workflow_status(workflow_id):
    async with aiopg.create_pool(dsn) as pool:
        # Non-blocking database operations
        pass
```

## Conclusion

The implemented telescoping delay mechanism and performance monitoring provide significant improvements for HPC environments running multiple rocotostat processes. Key benefits include:

- **Reduced System Load**: Exponential backoff prevents system overload
- **Better Resource Utilization**: More efficient use of database connections
- **Improved Observability**: Comprehensive performance metrics
- **Enhanced Reliability**: Graceful degradation under high load

Regular monitoring and adjustment of these parameters based on actual system performance will ensure optimal operation in production HPC environments.

## References

- [Rocoto Documentation](https://christopherwharrop.github.io/rocoto/)
- [HPC Performance Tuning Best Practices](https://hpc.nih.gov/docs/tuning.html)
- [Database Connection Pooling](https://docs.sqlalchemy.org/en/14/core/pooling.html)
- [Circuit Breaker Pattern](https://martinfowler.com/bliki/CircuitBreaker.html)