Param(
    [Parameter(Mandatory=$true)]
    [String]
    $fileName,
    [String]
    $xtraMessage
)


kaggle competitions submit -c m5-forecasting-accuracy -f $fileName -m "submission for team " + $xtraMessage
