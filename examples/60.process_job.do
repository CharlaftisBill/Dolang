MaxRetries i8 : 3
SystemName str : "BatchProcessor v1.0"

# processJob simulates working on a job.
# It returns a success message (string) or an error (failure return).
processJob (jobID int, baseLoad f64) string : {
	# Create a simulated load calculation
	calcLoad f64 : f64(jobID) * baseLoad

	if jobID < 0 {
		failure  NegativeJobId "invalid job ID: negative value"
	} else calcLoad > 100.0 {
		failure JobOverload "job overload: requires too much memory"
	}

	match jobID {
        0 {
            success "System Check Job Completed"
        },
        42 {
            success "Priority Job Completed"
        },
        * { 
            success Sprint("Standard Job %d Completed (Load: %.2f)", jobID, calcLoad)
        },
	}
}

main () : {
	print("\n--- Starting %s ---\n", SystemName)

    jobBatch [6]i32 : [i32: 10, -5, 0, 42, 999, 4]
	processedCount i8 = 0

	for index, jobID in jobBatch {
		print("Processing Item at index %d with ID %d...\n", index, jobID)

		result str : processJob(jobID, 2.5) catch {
            NegativeJobId {
                print(">> Critical data error, skipping to next item.")
				continue
            },
            JobOverload {
				print(">> System Overload detected! Stopping batch.")
                break
            },
        }

		print("Result:", result)
		processedCount += 1
	}

	print("\n--- Initiating Shutdown Sequence ---")
	retryAttempts u8 = 0
	
	while retryAttempts < MaxRetries {
		retryAttempts += 1
		
		if retryAttempts%2 == 0 {
			print("Shutdown retry attempt %d...\n", retryAttempts)
		} else {
			print("Checking background processes (Attempt %d)...\n", retryAttempts)
		}
	}

	print("Program Finished Successfully.")
}