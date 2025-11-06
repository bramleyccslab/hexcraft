<?php

	// Create a database connection
	$mysqli = mysqli_connect("localhost","eco_mdb_root","KZrQupBmNL", "eco_mdb1");

	if (mysqli_connect_errno($mysqli)) {
		echo "Failed to connect to MySQL: " . mysqli_connect_error();
	}

	// Get values passed from JS
	$ip = $_SERVER['REMOTE_ADDR'];
	$date = date('Y-m-d');
	//$subject = $_POST['subject'];
	$results = $_POST['results'];
	//$jsPsychResults = $_POST['jsPsychResults'];

	//Create a query
	$query = "INSERT INTO hexcraft_exp2 (ip, date, results) VALUES ('{$ip}', '{$date}', '{$results}')";
	
	//Do it
	mysqli_query($mysqli, $query);

	//Close connection
	mysqli_close($mysqli);

?>
