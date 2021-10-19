command! -nargs=1 -complete=custom,my_language_server#CompleteCustom -bar MyInstallLanguageServer call my_language_server#Install(<f-args>)
