<?php



$save_dir = __DIR__ . '/../data/';


$filename = $_POST['filename'];
$data = $_POST['filedata'];
$full_path = $save_dir . $stamp . '_' . $filename;



file_put_contents($full_path, $data);

?>



