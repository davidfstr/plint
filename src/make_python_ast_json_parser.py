import json


LOCATION_MEMBER = {
    'type': 'location',
    'name': 'location'
}


def nativize(type_name):
    if type_name.endswith('_list'):
        return type_name[:-len('_list')] + ' list'
    if type_name.endswith('_option'):
        return type_name[:-len('_option')] + ' option'
    return type_name


# Load Python.asdl.json, which is an adapted version of CPython's Python.asdl:
# https://docs.python.org/3.4/library/ast.html
with open('Python.asdl.json') as f:
    asdl = json.loads(f.read())

# Parse JSON to ASDL structure
variants = []
for variant_info in asdl:
    if len(variant_info) == 2:
        (variant_name, case_infos) = variant_info
        has_location = False
    elif len(variant_info) == 3:
        (variant_name, case_infos, variant_flag) = variant_info
        if variant_flag != '__LOCATION__':
            raise ValueError(
                'Unrecognized variant_flag in variant_info: %s' % repr(variant_info))
        has_location = True
    else:
        raise ValueError(
            'Unrecognized variant_info format: %s' % repr(variant_info))
    
    cases = []
    for case_info in case_infos:
        if len(case_info) % 2 != 1:
            raise ValueError(
                'Unrecognized case_info format: %s' % repr(case_info))
        
        case_name = case_info[0]
        members = []
        for i in range(1, len(case_info), 2):
            members.append({
                'type': case_info[i],
                'name': case_info[i+1]
            })
        
        cases.append({
            'name': case_name,
            'members': members
        })
    
    # If has only one case whose name matches the variant itself,
    # the single case can be inlined
    is_inline = \
        len(cases) == 1 and cases[0]['name'] == variant_name
    
    variants.append({
        'name': variant_name,
        'cases': cases,
        'has_location': has_location,
        'is_inline': is_inline
    })

# Generate header
print('''
open Core.Std
open Option.Monad_infix (* for `>>=` *)
open Yojson.Basic.Util  (* for `member` *)

(* === AST Types === *)
'''.strip('\n'))

# Generate regular type definitions
print('type')
for variant in variants:
    if not variant['is_inline']:
        print('  %s =' % variant['name'])
        for case in variant['cases']:
            if len(case['members']) > 0:
                print('    | %s of %s_%s' % (case['name'], variant['name'], case['name']))
            else:
                print('    | %s' % (case['name']))
        print('    and')
    
    for case in variant['cases']:
        if len(case['members']) == 0:
            continue
        
        if variant['is_inline']:
            print('  %s = {' % (variant['name']))
        else:
            print('  %s_%s = {' % (variant['name'], case['name']))
            
        for member in case['members']:
            print('    %s : %s;' % (member['name'], nativize(member['type'])))
        if variant['has_location']:
            member = LOCATION_MEMBER
            print('    %s : %s;' % (member['name'], member['type']))
        print('  } and')
    print('  ')
print('  %s = { lineno : int; col_offset : int } and' % LOCATION_MEMBER['type'])
print('  ')

# Generate builtin type definitions
print('''
  (* --- Builtins --- *)
  identifier = string

  with sexp
'''.strip('\n'))
print('')

# Generate regular parse functions
print('(* === Parse AST from JSON === *)')
print('let rec')
for variant in variants:
    print('  parse_%s json =' % variant['name'])
    print('    match json with')
    for case in variant['cases']:
        print('      | `List [`String "%s"; members_json%s] ->' % (case['name'], '; attributes_json' if variant['has_location'] else ''))
        if len(case['members']) == 0:
            print('        Some %s' % case['name'])
        else:
            for member in case['members']:
                print('        let %s_json = members_json |> member "%s" in' % (member['name'], member['name']))
            print('        ')
            
            for member in case['members']:
                print('        parse_%s %s_json >>= fun %s ->' % (member['type'], member['name'], member['name']))
            print('        ')
            
            if variant['has_location']:
                member = LOCATION_MEMBER
                print('        parse_%s attributes_json >>= fun %s ->' % (member['type'], member['name']))
                print('        ')
            
            if variant['is_inline']:
                print('        Some ({')
            else:
                print('        Some (%s {' % case['name'])
            for member in case['members']:
                print('          %s = %s;' % (member['name'], member['name']))
            if variant['has_location']:
                member = LOCATION_MEMBER
                print('          %s = %s;' % (member['name'], member['name']))
            print('        })')
        print('      ')
        
    # Add error handling for unrecognized input format
    print('''
      | `List [`String unknown_type; _; _] ->
        let () = printf "*** PyAst: unrecognized kind of %s: %%s\\n" unknown_type in
        None
      
      | _ ->
        None and
'''.strip('\n') % variant['name'])
    print('  ')
    
    # Add related list and option parsers
    print('''
  parse_%s_list json =
    match json with
      | `List item_jsons ->
        Option.all (List.map item_jsons parse_%s) >>= fun items ->
        Some items
      
      | _ ->
        None and
  
  parse_%s_option json =
    match json with
      | `Null ->
        Some None
      
      | _ ->
        parse_%s json >>= fun %s ->
        Some (Some %s) and
  
'''.strip('\n') % ((variant['name'],) * 6))

# Generate builtin parse functions
print('''
  (* --- Builtins --- *)
  
  parse_identifier json =
    match json with
      | `String string ->
        Some string
      
      | _ ->
        None and
  
  parse_string json =
    match json with
      | `String string ->
        Some string
      
      | _ ->
        None and
  
  parse_int json =
    match json with
      | `Int n ->
        Some n
      
      | _ ->
        None and
  
  parse_location attributes_json =
    match attributes_json with
      | `Assoc [("lineno", `Int lineno); ("col_offset", `Int col_offset)] ->
        Some { lineno = lineno; col_offset = col_offset }
      
      | _ ->
        None
'''.strip('\n'))

