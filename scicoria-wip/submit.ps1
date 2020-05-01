Param(
    [Parameter(Mandatory=$true, Position = 1)]
    [String]
    $File,
    [Parameter(Position = 2)]
    [String]
    $Message
)

$msg = "submission for team $xtraMessage"

kaggle competitions submit -c m5-forecasting-accuracy -f $file -m $msg
Write-Debug $File
Write-Debug $msg

