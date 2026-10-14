let () = print_string (Ir.render ~debug_info:true (Debug_fixture.module_ ~markers:true))
