# Prompt
Invoke-Expression (&starship init powershell)

# Fix colors for light mode
$ISETheme = @{
    ContinuationPrompt           = "`e[30m"
    Default                      = "`e[30m"
    Member                       = "`e[90m"
    Number                       = "`e[90m"
    Type                         = "`e[30m"
}
Set-PSReadLineOption -Colors $ISETheme

# Aliases for Windows compatibility
New-Alias -Name ac	-Value Add-Content
New-Alias -Name cat	-Value Get-Content
New-Alias -Name clear	-Value Clear-Host
New-Alias -Name cnsn	-Value Connect-PSSession
New-Alias -Name compare -Value Compare-Object
New-Alias -Name cp	-Value Copy-Item
New-Alias -Name cpp	-Value Copy-ItemProperty
New-Alias -Name diff	-Value Compare-Object
New-Alias -Name dnsn	-Value Disconnect-PSSession
New-Alias -Name gsv	-Value Get-Service
New-Alias -Name kill	-Value Stop-Process
New-Alias -Name ls	-Value Get-ChildItem
New-Alias -Name man	-Value help
New-Alias -Name mount	-Value New-PSDrive
New-Alias -Name mv	-Value Move-Item
New-Alias -Name ogv	-Value Out-GridView
New-Alias -Name ps	-Value Get-Process
New-Alias -Name rm	-Value Remove-Item
New-Alias -Name rmdir	-Value Remove-Item
New-Alias -Name sasv	-Value Start-Service
New-Alias -Name shcm	-Value Show-Command
New-Alias -Name sleep	-Value Start-Sleep
New-Alias -Name sort	-Value Sort-Object
New-Alias -Name start	-Value Start-Process
New-Alias -Name tee	-Value Tee-Object
New-Alias -Name which	-Value Get-Command
New-Alias -Name write	-Value Write-Output

# Extra Linux compatibility aliases
New-Alias -Name grep -Value Select-String

# Personal Aliases
New-Alias -Name l -Value Get-ChildItem
New-Alias -Name d -Value Get-ChildItem
New-Alias -Name c -Value Clear-Host
function n { Set-Location ~/.config/nix }
function e { Invoke-Expression $Env:EDITOR }
function nd { Invoke-Expression "nix develop" }
function Nixos-Update { Invoke-Expression 'nix flake update' }
function Nixos-Clean ($time) { Invoke-Expression "sudo nix-collect-garbage --delete-older-than $time" }
function Nixos-Superclean ($time) { Invoke-Expression "sudo nix-collect-garbage --delete-old $time" }
function javac { Invoke-Expression ("javac -Xdiags:verbose" + ($Args -join ' ')) }
function audio-dl ($link) { Invoke-Expression "yt-dlp -x -f bestaudio --audio-quality 0 --add-metadata --embed-thumbnail -o `"%(artist)s - %(title)s.%(ext)s`" '$link'" }

function Nixos-Test ($configuration) {
    Invoke-Expression "nixos-rebuild test --flake .#$configuration --use-remote-sudo"
}

function Nixos-Switch ($configuration) {
    Invoke-Expression "nixos-rebuild switch --flake .#$configuration --use-remote-sudo"
}

# fm = config.apps.fileManager.command;
# i = config.apps.photoViewer.command;

###############################################################

# powershell-nix-shell
# TODO make into module
# TODO add correct licensing (BSD 3-Clause "New" or "Revised" License)

$Env:NIX_SHELL_PLUGIN_DIR = $PSScriptRoot

If ($env:OS -eq 'Windows_NT') {
    Write-Host
    Write-Host '  WARNING: powershell-nix-shell does not work on Windows'
    Write-Host
}

If (!(Get-Command bash -ErrorAction SilentlyContinue)) {
    Write-Host
    Write-Host '  WARNING: bash is not installed.'
    Write-Host '  for powershell-nix-shell to work bash has to be in PATH'
    Write-Host
}

# extracts packages argument from args and passes them in $NIX_SHELL_PACKAGES variable.
Function Nix-Shell {
    $NIX_SHELL_PACKAGES = $Env:NIX_SHELL_PACKAGES

    # extract -p|--packages argument into NIX_SHELL_PACKAGES
    $IN_PACKAGES = $false
    $PURE = $false
    For ($i = 0; $i -lt $Args.Count; $i += 1) {
	$key = $Args[$i]
	# enter "--packages packages..." mode
	If (($key -eq "-p") -or ($key -eq "--packages")) {
	    $IN_PACKAGES = $true
	    If ($null -ne $NIX_SHELL_PACKAGES) {
		$NIX_SHELL_PACKAGES += ' '
	    }
	    $NIX_SHELL_PACKAGES += $Args[$i + 1]
	    $i += 1
	}
	# skip "--arg name value" argument
	Elseif ($key -eq "--arg")  {
	    $IN_PACKAGES = $false
	    $i += 2
	}
	Elseif ($key -eq "--pure")  {
	    $PURE = $true
	}
	# skip all other unary arguments
	Elseif ($key -like "-*")  {
	    $IN_PACKAGES = $false
	    $i += 1
	}
	# If we don't have any argument prefix we are either in package mode
	# or we have encountered the path argument
	Elseif ($IN_PACKAGES)  {
	    $NIX_SHELL_PACKAGES += " $key"
	}
    }

    # call real nix shell
    # if you use --pure you get bash
    If (!$PURE) {
	$Env:NIX_SHELL_PACKAGES = $NIX_SHELL_PACKAGES
	$Env:NIX_BUILD_SHELL = $Env:NIX_SHELL_PLUGIN_DIR + "/scripts/buildShellShim"
	$Env:NIX_EXECUTING_SHELL = "$PSHOME/pwsh -nologo"
    }
    Invoke-Expression ("/run/current-system/sw/bin/env nix-shell " + ($Args -join ' '))
}
