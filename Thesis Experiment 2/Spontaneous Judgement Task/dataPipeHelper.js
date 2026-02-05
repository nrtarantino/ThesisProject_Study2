// Function to send data to jsPsych data pipe
// Add this to your Test3.html file

function sendDataToPipe(csvData, experimentID = "1IQX1jXTAEW6") {
    // Generate unique filename with timestamp
    const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
    const filename = `test3_data_${timestamp}.csv`;
    
    // Convert CSV data to string if it's not already
    const dataAsString = typeof csvData === 'string' ? csvData : csvData;
    
    // Send to data pipe
    fetch("https://pipe.jspsych.org/api/data/", {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
            Accept: "*/*",
        },
        body: JSON.stringify({
            experimentID: experimentID,
            filename: filename,
            data: dataAsString,
        }),
    })
    .then(response => {
        if (response.ok) {
            console.log('Data successfully sent to data pipe');
            return response.json();
        } else {
            throw new Error('Failed to send data to pipe');
        }
    })
    .then(data => {
        console.log('Data pipe response:', data);
    })
    .catch(error => {
        console.error('Error sending data to pipe:', error);
        // Optionally show an alert or handle error
    });
}

