class ZCL_BC_DYNAM_KEY_CHECK definition
  public
  final
  create public .

    public section.

        constants:
            begin of c_fieldname,
                key type string value 'KEY',
                index type string value 'INDEX',
            end of c_fieldname.

        types:
            ty_t_indexes_for_key type standard table of i.

        class-methods:
            create_from_dbtab_key
                importing
                    iv_tabname type csequence
                returning
                    value(rv_result) type ref to ZCL_BC_DYNAM_KEY_CHECK.

        methods:

            constructor
                importing
                    is_key type data,

            add_key_and_index
                importing
                    is_data type data
                    iv_index type i,

            get_indexes_for_key
                importing
                    is_data type data
                exporting
                    et_indexes_for_key type ty_t_indexes_for_key.


    protected section.

        data:
            lv_ref_t_index_by_key type ref to data,
            lv_type_desc_s_index_by_key type ref to cl_abap_datadescr.

        types:
            begin of ty_s_index_by_key_refs,
                ref_s type ref to data, " referencia à estrutura
                ref_key type ref to data,
                ref_index type ref to i,
            end of ty_s_index_by_key_refs.

        methods:
            map_s_index_by_key_to_refs
                importing
                    iv_ref_s_data type ref to data
                returning
                    value(rv_result) type ref to ty_s_index_by_key_refs,

            create_s_index_by_key
                returning
                    value(rv_result) type ref to ty_s_index_by_key_refs.

    private section.
ENDCLASS.



CLASS ZCL_BC_DYNAM_KEY_CHECK IMPLEMENTATION.

    method constructor.

        DATA(lt_component) = VALUE cl_abap_structdescr=>component_table( ).

        APPEND INITIAL LINE TO lt_component ASSIGNING FIELD-SYMBOL(<ls_component>).
        <ls_component>-name = c_fieldname-key.
        <ls_component>-type ?= cl_abap_datadescr=>describe_by_data( is_key ).
        <ls_component>-as_include = abap_true.

        APPEND INITIAL LINE TO lt_component ASSIGNING <ls_component>.
        <ls_component>-name = c_fieldname-index.
        <ls_component>-type ?= cl_abap_datadescr=>describe_by_data( value i( ) ).



        lv_type_desc_s_index_by_key = cast cl_abap_datadescr(
          cl_abap_structdescr=>create(
            p_components = lt_component
          )
        ).

        data:
            lv_type_desc_t_table TYPE REF TO cl_abap_tabledescr.

        lv_type_desc_t_table = cl_abap_tabledescr=>create(
          p_line_type = lv_type_desc_s_index_by_key
          P_TABLE_KIND = cl_abap_tabledescr=>tablekind_sorted
          p_unique = abap_false
          p_key = value #(
            (
                name = c_fieldname-key
            )
          )
        ).

        create data lv_ref_t_index_by_key type handle lv_type_desc_t_table.

    endmethod.

    method map_s_index_by_key_to_refs.

        create data rv_result.

        assign iv_ref_s_data->(c_fieldname-key) to field-symbol(<ls_key>).
        assign iv_ref_s_data->(c_fieldname-index) to field-symbol(<lv_index>).

        rv_result->ref_s = iv_ref_s_data.
        rv_result->ref_key = ref #( <ls_key> ).
        rv_result->ref_index = ref #( <lv_index> ).

    endmethod.

    method create_s_index_by_key.

        data:
            lv_ref_s_data type ref to data.

        create data lv_ref_s_data type handle lv_type_desc_s_index_by_key.

        rv_result = map_s_index_by_key_to_refs( lv_ref_s_data ).

    endmethod.

    method add_key_and_index.

        data(lv_ref_s_index_by_key) = me->create_s_index_by_key( ).

        field-symbols:
            <ls_index_by_key> type data.

        assign lv_ref_s_index_by_key->ref_key->* to field-symbol(<ls_key>).
        move-corresponding is_data to <ls_key>.

        lv_ref_s_index_by_key->ref_index->* = iv_index.

        assign lv_ref_s_index_by_key->ref_s->* to field-symbol(<ls_s>).

        field-symbols:
            <lt_index_by_key> type sorted table.

        assign me->lv_ref_t_index_by_key->* to <lt_index_by_key>.

        insert <ls_s> into table <lt_index_by_key>.

    endmethod.

    method get_indexes_for_key.

        clear et_indexes_for_key.

        data(lv_ref_s_index_by_key) = me->create_s_index_by_key( ).

        assign lv_ref_s_index_by_key->ref_key->* to field-symbol(<ls_searched_key>).
        move-corresponding is_data to <ls_searched_key>.

        assign lv_ref_s_index_by_key->ref_s->* to field-symbol(<ls_searched_s>).

        field-symbols:
            <lt_index_by_key> type sorted table.

        assign me->lv_ref_t_index_by_key->* to <lt_index_by_key>.

        read table <lt_index_by_key> from <ls_searched_s> transporting no fields.

        check sy-subrc eq 0.

        data(lv_from) = sy-tabix.

        loop at <lt_index_by_key> assigning field-symbol(<ls_index_by_key>)
            from lv_from.

            data(lv_current) = map_s_index_by_key_to_refs( ref #( <ls_index_by_key> ) ).

            assign lv_current->ref_key->* to field-symbol(<ls_current_key>).

            if not ( <ls_searched_key> = <ls_current_key> ).
                exit.
            endif.

            append lv_current->ref_index->* to et_indexes_for_key.

        endloop.

    endmethod.

    method create_from_dbtab_key.

        select
            fieldname
        from
            dd03l
        into table
            @data(lt_key_fieldnames)
        where
            tabname = @iv_tabname and
            KEYFLAG = @abap_true
        order by
            position.

        DATA(lt_component) = VALUE cl_abap_structdescr=>component_table( ).

        loop at lt_key_fieldnames assigning field-symbol(<ls_key_fieldnames>).

            APPEND INITIAL LINE TO lt_component ASSIGNING FIELD-SYMBOL(<ls_component>).
            <ls_component>-name = <ls_key_fieldnames>-fieldname.
            <ls_component>-type ?= cl_abap_datadescr=>describe_by_name( |{ iv_tabname }-{ <ls_key_fieldnames>-fieldname }| ).

        endloop.

        data(lv_type_desc_s_key) = cast cl_abap_datadescr(
          cl_abap_structdescr=>create(
            p_components = lt_component
          )
        ).

        data:
            lv_ref_s_key type ref to data.

        create data lv_ref_s_key type handle lv_type_desc_s_key.

        assign lv_ref_s_key->* to field-symbol(<ls_key>).

        create object rv_result
            exporting
                is_key = <ls_key>.

    endmethod.

ENDCLASS.
