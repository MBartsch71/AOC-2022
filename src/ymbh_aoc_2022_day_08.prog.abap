REPORT ymbh_aoc_2022_day_08.



CLASS input_reader DEFINITION.
  PUBLIC SECTION.
    METHODS read_file_in_table RETURNING VALUE(result) TYPE stringtab.
ENDCLASS.

CLASS input_reader IMPLEMENTATION.

  METHOD read_file_in_table.
    DATA(file_reader) = NEW zcl_mbh_file_upload( |/Users/mbartsch71/github/AOC-2022/inputs/20221208| ).
    result = file_reader->file_upload_in_stringtab( ).
  ENDMETHOD.

ENDCLASS.

CLASS forest_tree DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING height TYPE i.
    METHODS height RETURNING VALUE(result) TYPE i.
    METHODS is_invisible RETURNING VALUE(result) TYPE abap_bool.
    METHODS set_invisible.
    METHODS set_scenic_score IMPORTING scenic_score TYPE i.
    METHODS get_scenic_score RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA tree_height TYPE i.
    DATA invisible TYPE abap_bool.
    DATA scenic_score TYPE i.
ENDCLASS.

CLASS forest_tree IMPLEMENTATION.

  METHOD constructor.
    tree_height = height.
  ENDMETHOD.

  METHOD height.
    result = tree_height.
  ENDMETHOD.

  METHOD is_invisible.
    result = invisible.
  ENDMETHOD.

  METHOD set_invisible.
    invisible = abap_true.
  ENDMETHOD.

  METHOD set_scenic_score.
    me->scenic_score = scenic_score.
  ENDMETHOD.

  METHOD get_scenic_score.
    result = scenic_score.
  ENDMETHOD.

ENDCLASS.

CLASS trees DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF tree_properties,
             height TYPE i,
             x_pos  TYPE i,
             y_pos  TYPE i,
           END OF tree_properties.
    TYPES: BEGIN OF tree_in_collection,
             x_pos TYPE i,
             y_pos TYPE i,
             tree  TYPE REF TO forest_tree,
           END OF tree_in_collection.
    TYPES trees TYPE SORTED TABLE OF tree_in_collection WITH UNIQUE KEY primary_key COMPONENTS x_pos y_pos.

    METHODS add IMPORTING tree_infos TYPE tree_properties.

    METHODS get_tree_by_coordinates IMPORTING x             TYPE i
                                              y             TYPE i
                                    RETURNING VALUE(result) TYPE REF TO forest_tree.

    METHODS get_border_trees_amount RETURNING VALUE(result) TYPE i.
    METHODS check_invisibility RETURNING VALUE(result) TYPE i.
    METHODS get_visible_trees
      RETURNING
        VALUE(result) TYPE i.
    METHODS check_visibility.
    METHODS get_highest_scenic_score
      RETURNING
        VALUE(result) TYPE i.


  PRIVATE SECTION.
    DATA tree_coll TYPE trees.

    METHODS left_border_trees RETURNING VALUE(result) TYPE i.

    METHODS right_border_trees RETURNING VALUE(result) TYPE i.

    METHODS top_border_trees RETURNING VALUE(result) TYPE i.

    METHODS bottom_border_trees RETURNING VALUE(result) TYPE i.

    METHODS get_right_border RETURNING VALUE(result) TYPE i.

    METHODS is_border_tree IMPORTING line          TYPE tree_in_collection
                           RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_top_border_tree IMPORTING line          TYPE tree_in_collection
                               RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_left_border_tree IMPORTING line          TYPE tree_in_collection
                                RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_bottom_border_tree IMPORTING line          TYPE tree_in_collection
                                  RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_right_border_tree IMPORTING line          TYPE tree_in_collection
                                 RETURNING VALUE(result) TYPE abap_bool.

    METHODS invisible_from_top_direction IMPORTING line          TYPE REF TO tree_in_collection
                                         RETURNING VALUE(result) TYPE abap_bool.

    METHODS invisible_from_left_direction IMPORTING line          TYPE REF TO tree_in_collection
                                          RETURNING VALUE(result) TYPE abap_bool.

    METHODS invisible_from_right_direction IMPORTING line          TYPE REF TO tree_in_collection
                                           RETURNING VALUE(result) TYPE abap_bool.

    METHODS invisible_from_bottom IMPORTING line          TYPE REF TO tree_in_collection
                                  RETURNING VALUE(result) TYPE abap_bool.

    METHODS get_top_visibility IMPORTING line          TYPE REF TO tree_in_collection
                               RETURNING VALUE(result) TYPE i.
    METHODS get_left_visibility IMPORTING line          TYPE REF TO tree_in_collection
                                RETURNING VALUE(result) TYPE i.
    METHODS get_right_visibility IMPORTING line          TYPE REF TO tree_in_collection
                                 RETURNING VALUE(result) TYPE i.
    METHODS get_bottom_visibility IMPORTING line          TYPE REF TO tree_in_collection
                                  RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS trees IMPLEMENTATION.

  METHOD add.
    tree_coll = VALUE #( BASE tree_coll ( x_pos = tree_infos-x_pos
                                          y_pos = tree_infos-y_pos
                                          tree  = NEW #( tree_infos-height ) ) ).
  ENDMETHOD.

  METHOD get_tree_by_coordinates.
    result = VALUE #( tree_coll[ x_pos = x y_pos = y ]-tree OPTIONAL ).
  ENDMETHOD.

  METHOD get_border_trees_amount.
    result = left_border_trees( ) +
             top_border_trees( ) +
             right_border_trees( ) +
             bottom_border_trees( ).
  ENDMETHOD.

  METHOD left_border_trees.
    result = REDUCE #( INIT sum = 0
                       FOR tree IN tree_coll WHERE ( x_pos = 1 )
                       NEXT sum = sum + 1 ).
  ENDMETHOD.

  METHOD right_border_trees.
    DATA(bottom_line) = tree_coll[ lines( tree_coll ) ]-y_pos.
    DATA(right_border) = REDUCE #( INIT sum = 0
                                   FOR tree IN tree_coll WHERE ( y_pos = bottom_line )
                                   NEXT sum = sum + 1 ).
    result = REDUCE #( INIT sum = 0
                       FOR tree IN tree_coll WHERE ( x_pos = right_border )
                       NEXT sum = sum + 1 ).
  ENDMETHOD.

  METHOD top_border_trees.
    result = REDUCE #( INIT sum = 0
                       FOR tree IN tree_coll WHERE ( x_pos = 1 )
                       NEXT sum = sum + 1 ).
    result = result - 2.
  ENDMETHOD.

  METHOD bottom_border_trees.
    DATA(bottom_border) = tree_coll[ lines( tree_coll ) ]-y_pos.
    result = REDUCE #( INIT sum = 0
                       FOR tree IN tree_coll WHERE ( y_pos = bottom_border )
                       NEXT sum = sum + 1 ).
    result = result - 2.
  ENDMETHOD.

  METHOD check_invisibility.
    LOOP AT tree_coll REFERENCE INTO DATA(line).
      IF is_border_tree( line->* ).
        CONTINUE.
      ENDIF.

      IF invisible_from_top_direction( line ) AND invisible_from_left_direction( line ) AND
         invisible_from_right_direction( line ) AND invisible_from_bottom( line ).
        DATA(tree) = get_tree_by_coordinates( x = line->x_pos y = line->y_pos ).
        tree->set_invisible( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD is_border_tree.
    result = xsdbool( is_top_border_tree( line )  OR
                      is_left_border_tree( line )  OR
                      is_bottom_border_tree( line ) OR
                      is_right_border_tree( line ) ).
  ENDMETHOD.

  METHOD is_top_border_tree.
    result = xsdbool( line-y_pos = 1 ).
  ENDMETHOD.

  METHOD is_left_border_tree.
    result = xsdbool( line-x_pos = 1 ).
  ENDMETHOD.

  METHOD is_bottom_border_tree.
    result = xsdbool( line-y_pos = lines( tree_coll ) ).
  ENDMETHOD.


  METHOD is_right_border_tree.
    DATA(right_border) = get_right_border( ).
    result = xsdbool( line-x_pos = right_border ).
  ENDMETHOD.

  METHOD invisible_from_top_direction.
    DATA(top_y_pos) = 1.
    DATA(check_y_pos) = line->y_pos - 1.
    DATA(my_height) = line->tree->height( ).
    DATA(check_over) = abap_false.
    WHILE check_y_pos >= top_y_pos OR check_over = abap_false.
      DATA(neighbout_tree) = get_tree_by_coordinates( x = line->x_pos y = check_y_pos ).
      IF neighbout_tree IS NOT BOUND.
        check_over = abap_true.
        CONTINUE.
      ENDIF.
      IF my_height <= neighbout_tree->height( ).
        result = abap_true.
        check_over = abap_true.
      ENDIF.
      check_y_pos = check_y_pos - 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD invisible_from_left_direction.
    DATA(topleft_x_pos) = 1.
    DATA(check_x_pos) = line->x_pos - 1.
    DATA(my_height) = line->tree->height( ).
    DATA(check_over) = abap_false.
    WHILE check_x_pos >= topleft_x_pos OR check_over = abap_false.
      DATA(neighbout_tree) = get_tree_by_coordinates( x = check_x_pos y = line->y_pos ).
      IF neighbout_tree IS NOT BOUND.
        check_over = abap_true.
        CONTINUE.
      ENDIF.
      IF my_height <= neighbout_tree->height( ).
        result = abap_true.
        check_over = abap_true.
      ENDIF.
      check_x_pos = check_x_pos - 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD invisible_from_right_direction.
    DATA(topright_x_pos) = get_right_border( ).
    DATA(check_x_pos) = line->x_pos + 1.
    DATA(my_height) = line->tree->height( ).
    DATA(check_over) = abap_false.
    WHILE check_x_pos <= topright_x_pos OR check_over = abap_false.
      DATA(neighbout_tree) = get_tree_by_coordinates( x = check_x_pos y = line->y_pos ).
      IF neighbout_tree IS NOT BOUND.
        check_over = abap_true.
        CONTINUE.
      ENDIF.
      IF my_height <= neighbout_tree->height( ).
        result = abap_true.
        check_over = abap_true.
      ENDIF.
      check_x_pos = check_x_pos + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD invisible_from_bottom.
    DATA(topbottom_y_pos) = tree_coll[ lines( tree_coll ) ]-y_pos.
    DATA(check_y_pos) = line->y_pos + 1.
    DATA(my_height) = line->tree->height( ).
    DATA(check_over) = abap_false.
    WHILE check_y_pos <= topbottom_y_pos OR check_over = abap_false.
      DATA(neighbout_tree) = get_tree_by_coordinates( x = line->x_pos y = check_y_pos ).
      IF neighbout_tree IS NOT BOUND.
        check_over = abap_true.
        CONTINUE.
      ENDIF.
      IF my_height <= neighbout_tree->height( ).
        result = abap_true.
        check_over = abap_true.
      ENDIF.
      check_y_pos = check_y_pos + 1.
    ENDWHILE.
  ENDMETHOD.


  METHOD get_right_border.
    DATA(bottom_tree_line) = tree_coll[ lines( tree_coll ) ]-y_pos.
    result = REDUCE #( INIT sum = 0
                       FOR tree IN tree_coll WHERE ( y_pos = bottom_tree_line )
                       NEXT sum = sum + 1 ).
  ENDMETHOD.


  METHOD get_visible_trees.
    LOOP AT tree_coll REFERENCE INTO DATA(line).
      IF line->tree->is_invisible( ) = abap_false.
        result = result + 1.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD check_visibility.
    LOOP AT tree_coll REFERENCE INTO DATA(line).
      DATA(top_visiblity) = get_top_visibility( line ).
      DATA(bottom_visibility) = get_bottom_visibility( line ).
      DATA(left_visibility) = get_left_visibility( line ).
      DATA(right_visibility) = get_right_visibility( line ).

      DATA(scenic_score) = top_visiblity * bottom_visibility * left_visibility * right_visibility.
      line->tree->set_scenic_score( scenic_score ).
    ENDLOOP.

  ENDMETHOD.

  METHOD get_top_visibility.
    DATA(view_score) = 0.
    DATA(topmost_y) = 1.
    DATA(check_tree_y) = line->y_pos - 1.
    IF line->y_pos = topmost_y.
      RETURN.
    ENDIF.
    WHILE check_tree_y >= topmost_y.
      DATA(check_tree) = get_tree_by_coordinates( x = line->x_pos y = check_tree_y ).
      IF check_tree->height( ) >= line->tree->height( ).
        result = view_score + 1.
        RETURN.
      ENDIF.
      view_score = view_score + 1.
      check_tree_y = check_tree_y - 1.
    ENDWHILE.
    result = view_score.
  ENDMETHOD.

  METHOD get_bottom_visibility.
    DATA(view_score) = 0.
    DATA(bottommost_y) = tree_coll[ lines( tree_coll ) ]-y_pos.
    DATA(check_tree_y) = line->y_pos + 1.
    IF line->y_pos = bottommost_y.
      RETURN.
    ENDIF.
    WHILE check_tree_y <= bottommost_y.
      DATA(check_tree) = get_tree_by_coordinates( x = line->x_pos y = check_tree_y ).
      IF check_tree->height( ) >= line->tree->height( ).
        result = view_score + 1.
        RETURN.
      ENDIF.
      view_score = view_score + 1.
      check_tree_y = check_tree_y + 1.
    ENDWHILE.
    result = view_score.
  ENDMETHOD.

  METHOD get_left_visibility.
    DATA(view_score) = 0.
    DATA(leftmost_x) = 1.
    DATA(check_tree_x) = line->x_pos - 1.
    IF line->x_pos = leftmost_x.
      RETURN.
    ENDIF.
    WHILE check_tree_x >= leftmost_x.
      DATA(check_tree) = get_tree_by_coordinates( x = check_tree_x y = line->y_pos ).
      IF check_tree->height( ) >= line->tree->height( ).
        result = view_score + 1.
        RETURN.
      ENDIF.
      view_score = view_score + 1.
      check_tree_x = check_tree_x - 1.
    ENDWHILE.
    result = view_score.
  ENDMETHOD.

  METHOD get_right_visibility.
    DATA(view_score) = 0.
    DATA(rightmost_x) = get_right_border( ).
    DATA(check_tree_x) = line->x_pos + 1.
    IF line->x_pos = rightmost_x.
      RETURN.
    ENDIF.
    WHILE check_tree_x <= rightmost_x.
      DATA(check_tree) = get_tree_by_coordinates( x = check_tree_x y = line->y_pos ).
      IF check_tree->height( ) >= line->tree->height( ).
        result = view_score + 1.
        RETURN.
      ENDIF.
      view_score = view_score + 1.
      check_tree_x = check_tree_x + 1.
    ENDWHILE.
    result = view_score.
  ENDMETHOD.


  METHOD get_highest_scenic_score.
    LOOP AT tree_coll REFERENCE INTO DATA(line).
      DATA(scenic_score) = line->tree->get_scenic_score( ).
      IF scenic_score > result.
        result = scenic_score.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS tree DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO forest_tree.
    METHODS create_tree_object FOR TESTING.
    METHODS get_height_of_tree FOR TESTING.

ENDCLASS.

CLASS tree IMPLEMENTATION.

  METHOD create_tree_object.
    cut = NEW #( 2 ).
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD get_height_of_tree.
    cut = NEW #( 4 ).
    cl_abap_unit_assert=>assert_equals( exp = 4 act = cut->height( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS input_processor DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING input TYPE stringtab.
    METHODS get_trees
      RETURNING
        VALUE(result) TYPE REF TO trees.

  PRIVATE SECTION.
    DATA input TYPE stringtab.
    DATA trees TYPE REF TO trees.
    METHODS process_input.
ENDCLASS.

CLASS input_processor IMPLEMENTATION.

  METHOD constructor.
    me->input = input.
    trees = NEW #( ).
    process_input( ).
  ENDMETHOD.

  METHOD process_input.
    LOOP AT input REFERENCE INTO DATA(line).
      DATA(y_pos) = sy-tabix.
      DATA(line_length) = strlen( line->* ).
      DATA(read_pos) = 0.
      WHILE read_pos < line_length.
        DATA(height) = substring( val = line->* off = read_pos len = 1 ).
        trees->add( VALUE #( x_pos = read_pos + 1 y_pos = y_pos height = height ) ).
        read_pos = read_pos + 1.
      ENDWHILE.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_trees.
    result = trees.
  ENDMETHOD.

ENDCLASS.

CLASS tc_tree_collection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: BEGIN OF tree_properties,
             height TYPE i,
             x_pos  TYPE i,
             y_pos  TYPE i,
           END OF tree_properties.
    DATA cut TYPE REF TO trees.

    METHODS setup.

    METHODS create_object               FOR TESTING.
    METHODS store_tree_with_coordinates FOR TESTING.
    METHODS request_a_non_existing_tree FOR TESTING.

    METHODS get_border_trees_amount     FOR TESTING.
    METHODS get_invisible_tree          FOR TESTING.

    METHODS count_invisible_trees       FOR TESTING.

    METHODS get_scenic_score_of_tree    FOR TESTING.
    METHODS get_highest_scenic_score    FOR TESTING.

ENDCLASS.

CLASS tc_tree_collection IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    cut->add( VALUE #( x_pos = 1 y_pos = 1 height = 3 ) ).
    cut->add( VALUE #( x_pos = 2 y_pos = 1 height = 0 ) ).
    cut->add( VALUE #( x_pos = 3 y_pos = 1 height = 3 ) ).
    cut->add( VALUE #( x_pos = 4 y_pos = 1 height = 7 ) ).
    cut->add( VALUE #( x_pos = 5 y_pos = 1 height = 3 ) ).
    cut->add( VALUE #( x_pos = 1 y_pos = 2 height = 2 ) ).
    cut->add( VALUE #( x_pos = 2 y_pos = 2 height = 5 ) ).
    cut->add( VALUE #( x_pos = 3 y_pos = 2 height = 5 ) ).
    cut->add( VALUE #( x_pos = 4 y_pos = 2 height = 1 ) ).
    cut->add( VALUE #( x_pos = 5 y_pos = 2 height = 2 ) ).
    cut->add( VALUE #( x_pos = 1 y_pos = 3 height = 6 ) ).
    cut->add( VALUE #( x_pos = 2 y_pos = 3 height = 5 ) ).
    cut->add( VALUE #( x_pos = 3 y_pos = 3 height = 3 ) ).
    cut->add( VALUE #( x_pos = 4 y_pos = 3 height = 3 ) ).
    cut->add( VALUE #( x_pos = 5 y_pos = 3 height = 2 ) ).
    cut->add( VALUE #( x_pos = 1 y_pos = 4 height = 3 ) ).
    cut->add( VALUE #( x_pos = 2 y_pos = 4 height = 3 ) ).
    cut->add( VALUE #( x_pos = 3 y_pos = 4 height = 5 ) ).
    cut->add( VALUE #( x_pos = 4 y_pos = 4 height = 4 ) ).
    cut->add( VALUE #( x_pos = 5 y_pos = 4 height = 9 ) ).
    cut->add( VALUE #( x_pos = 1 y_pos = 5 height = 3 ) ).
    cut->add( VALUE #( x_pos = 2 y_pos = 5 height = 5 ) ).
    cut->add( VALUE #( x_pos = 3 y_pos = 5 height = 3 ) ).
    cut->add( VALUE #( x_pos = 4 y_pos = 5 height = 9 ) ).
    cut->add( VALUE #( x_pos = 5 y_pos = 5 height = 0 ) ).
  ENDMETHOD.

  METHOD create_object.
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD store_tree_with_coordinates.
    DATA(tree) = cut->get_tree_by_coordinates( x = 1 y = 3 ).
    cl_abap_unit_assert=>assert_equals( exp = 6 act = tree->height( )  ).
  ENDMETHOD.

  METHOD request_a_non_existing_tree.
    DATA(tree) = cut->get_tree_by_coordinates( x = 6 y = 1 ).
    cl_abap_unit_assert=>assert_not_bound( act = tree ).
  ENDMETHOD.

  METHOD get_border_trees_amount.
    cl_abap_unit_assert=>assert_equals( exp = 16 act = cut->get_border_trees_amount( )  ).
  ENDMETHOD.

  METHOD get_invisible_tree.
    cut->check_invisibility( ).
    DATA(tree) = cut->get_tree_by_coordinates( x = 3 y = 3 ).
    cl_abap_unit_assert=>assert_true( act = tree->is_invisible( ) ).
  ENDMETHOD.

  METHOD count_invisible_trees.
    cut->check_invisibility( ).
    cl_abap_unit_assert=>assert_equals( exp = 21 act = cut->get_visible_trees( ) ).
  ENDMETHOD.

  METHOD get_scenic_score_of_tree.
    cut->check_visibility( ).
    DATA(tree) = cut->get_tree_by_coordinates( x = 3 y = 4 ).
    cl_abap_unit_assert=>assert_equals( exp = 8 act = tree->get_scenic_score( )  ).
  ENDMETHOD.

  METHOD get_highest_scenic_score.
    cut->check_visibility( ).
    cl_abap_unit_assert=>assert_equals( exp = 8 act = cut->get_highest_scenic_score( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_input_processor DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO input_processor.

    METHODS setup.

    METHODS create_tree_coll_from_input FOR TESTING.
    METHODS get_specific_tree FOR TESTING.

ENDCLASS.

CLASS tc_input_processor IMPLEMENTATION.

  METHOD setup.
    DATA(input) = VALUE stringtab( ( |30373| )
                                   ( |25512| )
                                   ( |65332| )
                                   ( |33549| )
                                   ( |35390| ) ) .
    cut = NEW #( input ).
  ENDMETHOD.

  METHOD create_tree_coll_from_input.
    cl_abap_unit_assert=>assert_bound( act = cut->get_trees( ) msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD get_specific_tree.
    DATA(trees) = cut->get_trees( ).
    DATA(tree) = trees->get_tree_by_coordinates( x = 3 y = 2 ).
    cl_abap_unit_assert=>assert_equals( exp = 5 act = tree->height( )  ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input) = NEW input_reader( )->read_file_in_table( ).

  DATA(input_processor) = NEW input_processor( input ).
  DATA(trees) = input_processor->get_trees( ).

  trees->check_invisibility( ).
  WRITE / |Solution part 1: { trees->get_visible_trees( ) }|.

  trees->check_visibility( ).
  WRITE / |Solution part 2: { trees->get_highest_scenic_score( ) }|.
