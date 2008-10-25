# add shared-copy

shared_destination = "gking@common-lisp.net:/project/cl-containers/public_html/shared/"
shared-copy: FORCE
	rcopy website/source/shared/ $(shared_destination)
	@$(run_postactions)
