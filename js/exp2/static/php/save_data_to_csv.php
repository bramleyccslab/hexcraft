<?php

// the $_POST[] array will contain the passed in filename and data

// the directory "data" is writable by the server (chmod 777)

$save_dir = __DIR__ . '/../data/';


$filename = $_POST['filename'];
$data = $_POST['filedata'];
$full_path = $save_dir . $stamp . '_' . $filename;


// write the file to disk

file_put_contents($full_path, $data);

?>



