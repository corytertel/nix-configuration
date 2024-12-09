# # Prompt
# Invoke-Expression (&starship init powershell)

# # Fix colors for light mode
# $ISETheme = @{
#     ContinuationPrompt           = "`e[30m"
#     Default                      = "`e[30m"
#     Member                       = "`e[90m"
#     Number                       = "`e[90m"
#     Type                         = "`e[30m"
# }
# Set-PSReadLineOption -Colors $ISETheme

# Emacs-like binds
# Set-PSReadLineOption -EditMode Emacs

# Enable the same aliases as Windows PowerShell for compatibility
New-Alias -Name ac -Value Add-Content
New-Alias -Name asnp -Value Add-PSSnapin
New-Alias -Name cat -Value Get-Content
New-Alias -Name CFS -Value ConvertFrom-String
New-Alias -Name clear -Value Clear-Host
New-Alias -Name cnsn -Value Connect-PSSession
New-Alias -Name compare -Value Compare-Object
New-Alias -Name cp -Value Copy-Item
New-Alias -Name cpp -Value Copy-ItemProperty
New-Alias -Name curl -Value Invoke-WebRequest
New-Alias -Name diff -Value Compare-Object
New-Alias -Name dnsn -Value Disconnect-PSSession
New-Alias -Name epsn -Value Export-PSSession
New-Alias -Name gin -Value Get-ComputerInfo
New-Alias -Name gsnp -Value Get-PSSnapin
New-Alias -Name gsv -Value Get-Service
New-Alias -Name gwmi -Value Get-WmiObject
New-Alias -Name ipsn -Value Import-PSSession
New-Alias -Name iwmi -Value Invoke-WmiMethod
New-Alias -Name kill -Value Stop-Process
New-Alias -Name lp -Value Out-Printer
New-Alias -Name ls -Value Get-ChildItem
New-Alias -Name man -Value help
New-Alias -Name mount -Value New-PSDrive
New-Alias -Name mv -Value Move-Item
New-Alias -Name npssc -Value New-PSSessionConfigurationFile
New-Alias -Name ogv -Value Out-GridView
New-Alias -Name ps -Value Get-Process
New-Alias -Name rm -Value Remove-Item
New-Alias -Name rmdir -Value Remove-Item
New-Alias -Name rsnp -Value Remove-PSSnapin
New-Alias -Name rujb -Value Resume-Job
New-Alias -Name rwmi -Value Remove-WmiObject
New-Alias -Name sasv -Value Start-Service
New-Alias -Name sc -Value Set-Content
New-Alias -Name shcm -Value Show-Command
New-Alias -Name sleep -Value Start-Sleep
New-Alias -Name sort -Value Sort-Object
New-Alias -Name spsv -Value Stop-Service
New-Alias -Name start -Value Start-Process
New-Alias -Name stz -Value Set-TimeZone
New-Alias -Name sujb -Value Suspend-Job
New-Alias -Name swmi -Value Set-WmiInstance
New-Alias -Name tee -Value Tee-Object
New-Alias -Name trcm -Value Trace-Command
New-Alias -Name wget -Value Invoke-WebRequest
New-Alias -Name write -Value Write-Output

# Extra Linux compatibility aliases
New-Alias -Name grep -Value Select-String

# # Personal Aliases
# New-Alias -Name l -Value Get-ChildItem
# New-Alias -Name d -Value Get-ChildItem
# New-Alias -Name c -Value Clear-Host
# function n { Set-Location ~/.config/nix }
# function e { Invoke-Expression $Env:EDITOR }
# function nd { Invoke-Expression "nix develop" }
# function Nixos-Update { Invoke-Expression 'nix flake update' }
# function Nixos-Clean ($time) { Invoke-Expression "sudo nix-collect-garbage --delete-older-than $time" }
# function Nixos-Superclean ($time) { Invoke-Expression "sudo nix-collect-garbage --delete-old $time" }
# function javac { Invoke-Expression ("javac -Xdiags:verbose" + ($Args -join ' ')) }
# function audio-dl ($link) { Invoke-Expression "yt-dlp -x -f bestaudio --audio-quality 0 --add-metadata --embed-thumbnail -o `"%(artist)s - %(title)s.%(ext)s`" '$link'" }

# function Nixos-Test ($configuration) {
#     Invoke-Expression "nixos-rebuild test --flake .#$configuration --use-remote-sudo"
# }

# function Nixos-Switch ($configuration) {
#     Invoke-Expression "nixos-rebuild switch --flake .#$configuration --use-remote-sudo"
# }

# # fm = config.apps.fileManager.command;
# # i = config.apps.photoViewer.command;
