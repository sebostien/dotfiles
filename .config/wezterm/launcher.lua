return function(config)
	config.launch_menu = {
		{ label = "Cargo build release", args = { "cargo", "build", "--release" } },
		{ label = "Cargo build debug", args = { "cargo", "build" } },
		{
			label = "Cargo enable backtrace",
			args = { "zsh" },
			set_environment_variables = { RUST_BACKTRACE = "1" },
		},
		{ label = "Calculator", args = { "kalker" } },
	}
end
