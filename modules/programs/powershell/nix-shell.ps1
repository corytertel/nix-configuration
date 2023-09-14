#!/run/current-system/sw/bin/env pwsh

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
    While ($Args.Count -gt 0) {
	$key = $Args[0]
	# enter "--packages packages..." mode
	If (($key -eq "-p") -or ($key -eq "--packages")) {
	    $IN_PACKAGES = $true
	    If ($null -ne $NIX_SHELL_PACKAGES) {
		$NIX_SHELL_PACKAGES += ' '
	    }
	    $NIX_SHELL_PACKAGES += $Args[1]
	    $Args = $Args | Select-Object -Skip 1
	}
	# skip "--arg name value" argument
	Elseif ($key -eq "--arg")  {
	    $IN_PACKAGES = $false
	    $Args = $Args | Select-Object -Skip 2
	}
	Elseif ($key -eq "--pure")  {
	    $PURE = $true
	}
	# skip all other unary arguments
	Elseif ($key -like "-*")  {
	    $IN_PACKAGES = $false
	    $Args = $Args | Select-Object -Skip 1
	}
	# If we don't have any argument prefix we are either in package mode
	# or we have encountered the path argument
	Elseif ($IN_PACKAGES)  {
	    $NIX_SHELL_PACKAGES += " $key"
	}
	$Args = $Args | Select-Object -Skip 1
    }

    # call real nix shell
    # if you use --pure you get bash
    If (!$PURE) {
	$Env:NIX_SHELL_PACKAGES = $NIX_SHELL_PACKAGES
	# TODO have buildshellshim in the correct directory instead of PATH
	# $Env:NIX_BUILD_SHELL = $Env:NIX_SHELL_PLUGIN_DIR + "/scripts/buildShellShim"
	$Env:NIX_BUILD_SHELL = "/run/current-system/sw/bin/env buildShellShim"
	$Env:NIX_EXECUTING_SHELL = "$PSHOME/pwsh"
    }
    Invoke-Expression ("/run/current-system/sw/bin/env nix-shell " + ($Args -join ' '))
}
