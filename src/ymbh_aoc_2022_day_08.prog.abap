REPORT ymbh_aoc_2022_day_08.



CLASS tree DEFINITION DEFERRED.
CLASS trees DEFINITION DEFERRED.

INTERFACE if_trees.
  TYPES: BEGIN OF tree_properties,
           height TYPE i,
           x_pos  TYPE i,
           y_pos  TYPE i,
         END OF tree_properties.
  TYPES tree_properties_tab TYPE STANDARD TABLE OF tree_properties WITH EMPTY KEY.

  TYPES: BEGIN OF tree_in_collection,
           x_pos TYPE i,
           y_pos TYPE i,
           tree  TYPE REF TO tree,
         END OF tree_in_collection.
  TYPES trees TYPE SORTED TABLE OF tree_in_collection WITH UNIQUE KEY primary_key COMPONENTS x_pos y_pos.

ENDINTERFACE.


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

CLASS tree DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING height TYPE i.

    METHODS set_invisible.
    METHODS set_scenic_score IMPORTING scenic_score TYPE i.

    METHODS height RETURNING VALUE(result) TYPE i.
    METHODS scenic_score RETURNING VALUE(result) TYPE i.
    METHODS is_invisible RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    DATA tree_height TYPE i.
    DATA invisible TYPE abap_bool.
    DATA score TYPE i.

ENDCLASS.

CLASS tree IMPLEMENTATION.

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
    score = scenic_score.
  ENDMETHOD.

  METHOD scenic_score.
    result = score.
  ENDMETHOD.

ENDCLASS.

CLASS forest DEFINITION.

  PUBLIC SECTION.
    METHODS constructor IMPORTING trees TYPE REF TO trees.

    METHODS build.

    METHODS tree_is_in_border IMPORTING tree_properties TYPE if_trees=>tree_in_collection
                              RETURNING VALUE(result)   TYPE abap_bool.

    METHODS northward_trees IMPORTING coordinates   TYPE if_trees=>tree_properties
                            RETURNING VALUE(result) TYPE if_trees=>tree_properties_tab.

    METHODS eastward_trees IMPORTING coordinates   TYPE if_trees=>tree_properties
                           RETURNING VALUE(result) TYPE if_trees=>tree_properties_tab.

    METHODS southward_trees IMPORTING coordinates   TYPE if_trees=>tree_properties
                            RETURNING VALUE(result) TYPE if_trees=>tree_properties_tab.

    METHODS westward_trees IMPORTING coordinates   TYPE if_trees=>tree_properties
                           RETURNING VALUE(result) TYPE if_trees=>tree_properties_tab.

  PRIVATE SECTION.
    TYPES: BEGIN OF forest_borders,
             top    TYPE i,
             left   TYPE i,
             right  TYPE i,
             bottom TYPE i,
           END OF forest_borders.
    DATA trees TYPE REF TO trees.
    DATA borders TYPE forest_borders.

ENDCLASS.

CLASS trees DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF borders,
             left   TYPE i,
             right  TYPE i,
             top    TYPE i,
             bottom TYPE i,
           END OF borders.

    METHODS add IMPORTING tree_infos TYPE if_trees=>tree_properties.

    METHODS get_tree_by_coordinates IMPORTING x             TYPE i
                                              y             TYPE i
                                    RETURNING VALUE(result) TYPE REF TO tree.

    METHODS get_visible_trees RETURNING VALUE(result) TYPE i.

    METHODS get_highest_scenic_score RETURNING VALUE(result) TYPE i.

    METHODS has_next RETURNING VALUE(result) TYPE abap_bool.
    METHODS get_next RETURNING VALUE(result) TYPE if_trees=>tree_in_collection.

    METHODS get_bottom_border RETURNING VALUE(result) TYPE i.
    METHODS get_right_border RETURNING VALUE(result) TYPE i.

    METHODS build_forest.
    METHODS reset_current_tree.

  PRIVATE SECTION.
    DATA tree_coll TYPE if_trees=>trees.
    DATA forest TYPE REF TO forest.
    DATA current_tree TYPE i VALUE 0.

ENDCLASS.

CLASS forest IMPLEMENTATION.

  METHOD constructor.
    me->trees = trees.
  ENDMETHOD.

  METHOD build.
    borders = VALUE #( top = 1 left = 1
                       bottom = trees->get_bottom_border( )
                       right = trees->get_right_border( ) ).
  ENDMETHOD.

  METHOD tree_is_in_border.
    result = xsdbool( tree_properties-x_pos = borders-left OR
                      tree_properties-x_pos = borders-right OR
                      tree_properties-y_pos = borders-top OR
                      tree_properties-y_pos = borders-bottom ).
  ENDMETHOD.

  METHOD northward_trees.
    result = VALUE #( FOR i = coordinates-y_pos - 1 THEN i - 1 WHILE i >= borders-top
                        LET target_tree = trees->get_tree_by_coordinates( x = coordinates-x_pos y = i )
                        IN ( x_pos = coordinates-x_pos y_pos = i height = target_tree->height( ) ) ).
  ENDMETHOD.

  METHOD eastward_trees.
    result = VALUE #( FOR i = coordinates-x_pos + 1 THEN i + 1 WHILE i <= borders-right
                        LET target_tree = trees->get_tree_by_coordinates( x = i y = coordinates-y_pos )
                        IN ( x_pos = i y_pos = coordinates-y_pos height = target_tree->height( ) ) ).
  ENDMETHOD.

  METHOD southward_trees.
    result = VALUE #( FOR i = coordinates-y_pos + 1 THEN i + 1 WHILE i <= borders-bottom
                        LET target_tree = trees->get_tree_by_coordinates( x = coordinates-x_pos y = i )
                        IN ( x_pos = coordinates-x_pos y_pos = i height = target_tree->height( ) ) ).
  ENDMETHOD.

  METHOD westward_trees.
    result = VALUE #( FOR i = coordinates-x_pos - 1 THEN i - 1 WHILE i >= borders-left
                        LET target_tree = trees->get_tree_by_coordinates( x = i y = coordinates-y_pos )
                        IN ( x_pos = i y_pos = coordinates-y_pos height = target_tree->height( ) ) ).
  ENDMETHOD.

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

  METHOD get_visible_trees.
    result = REDUCE #( INIT sum = 0
                        FOR line IN tree_coll
                        NEXT sum = COND #( WHEN NOT line-tree->is_invisible( ) THEN sum + 1
                                           ELSE sum ) ).
  ENDMETHOD.

  METHOD get_highest_scenic_score.
    result = REDUCE #( INIT sum = 0
                       FOR line IN tree_coll
                       NEXT sum = COND #( WHEN line-tree->scenic_score( ) > sum
                                            THEN line-tree->scenic_score( )
                                            ELSE sum ) ).
  ENDMETHOD.

  METHOD get_right_border.
    DATA(bottom_tree_line) = tree_coll[ lines( tree_coll ) ]-y_pos.
    result = REDUCE #( INIT sum = 0
                       FOR tree IN tree_coll WHERE ( y_pos = bottom_tree_line )
                       NEXT sum = sum + 1 ).
  ENDMETHOD.

  METHOD get_bottom_border.
    result = tree_coll[ lines( tree_coll ) ]-y_pos.
  ENDMETHOD.

  METHOD build_forest.
    forest = NEW #( me ).
  ENDMETHOD.

  METHOD has_next.
    result = xsdbool( line_exists( tree_coll[ current_tree + 1 ] )  ).
  ENDMETHOD.

  METHOD get_next.
    current_tree = current_tree + 1.
    result = tree_coll[ current_tree ].
  ENDMETHOD.

  METHOD reset_current_tree.
    current_tree = 0.
  ENDMETHOD.

ENDCLASS.

CLASS surveyor DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING forest TYPE REF TO forest.

    METHODS check_invisibility IMPORTING trees TYPE REF TO trees.

    METHODS check_visibility IMPORTING trees TYPE REF TO trees.

  PRIVATE SECTION.
    DATA forest TYPE REF TO forest.

    METHODS check_hiding_potential IMPORTING lane          TYPE if_trees=>tree_properties_tab
                                             tree          TYPE REF TO tree
                                   RETURNING VALUE(result) TYPE abap_bool.

    METHODS scenic_score_potential IMPORTING lane          TYPE if_trees=>tree_properties_tab
                                             tree          TYPE REF TO tree
                                   RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS surveyor IMPLEMENTATION.

  METHOD constructor.
    me->forest = forest.
  ENDMETHOD.

  METHOD check_invisibility.

    WHILE trees->has_next( ).
      DATA(tree_entry) = trees->get_next( ).
      DATA(coordinates) = VALUE if_trees=>tree_properties( x_pos = tree_entry-x_pos y_pos = tree_entry-y_pos ).

      DATA(northward_trees) = forest->northward_trees( coordinates ).
      DATA(north_invisible) = check_hiding_potential( lane = northward_trees tree = tree_entry-tree ).

      DATA(eastward_trees) = forest->eastward_trees( coordinates ).
      DATA(east_invisible) = check_hiding_potential( lane = eastward_trees tree = tree_entry-tree ).

      DATA(southward_trees) = forest->southward_trees( coordinates ).
      DATA(south_invisible) = check_hiding_potential( lane = southward_trees tree = tree_entry-tree ).

      DATA(westward_trees) = forest->westward_trees( coordinates ).
      DATA(west_invisible) = check_hiding_potential( lane = westward_trees tree = tree_entry-tree ).

      IF north_invisible = abap_true AND
         east_invisible  = abap_true AND
         south_invisible = abap_true AND
         west_invisible  = abap_true.
        tree_entry-tree->set_invisible( ).
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD check_visibility.
    WHILE trees->has_next( ).
      DATA(tree_entry) = trees->get_next( ).
      DATA(coordinates) = VALUE if_trees=>tree_properties( x_pos = tree_entry-x_pos y_pos = tree_entry-y_pos ).

      DATA(northward_trees) = forest->northward_trees( coordinates ).
      SORT northward_trees BY y_pos DESCENDING.
      DATA(north_scenic_score) = scenic_score_potential( lane = northward_trees tree = tree_entry-tree ).

      DATA(eastward_trees) = forest->eastward_trees( coordinates ).
      DATA(east_scenic_score) = scenic_score_potential( lane = eastward_trees tree = tree_entry-tree ).

      DATA(southward_trees) = forest->southward_trees( coordinates ).
      DATA(south_scenic_score) = scenic_score_potential( lane = southward_trees tree = tree_entry-tree ).

      DATA(westward_trees) = forest->westward_trees( coordinates ).
      DATA(west_scenic_score) = scenic_score_potential( lane = westward_trees tree = tree_entry-tree ).

      DATA(scenic_score) = north_scenic_score * east_scenic_score * south_scenic_score * west_scenic_score.
      tree_entry-tree->set_scenic_score( scenic_score ).
    ENDWHILE.
  ENDMETHOD.

  METHOD check_hiding_potential.
    LOOP AT lane REFERENCE INTO DATA(lane_item).
      IF tree->height( ) <= lane_item->height.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD scenic_score_potential.
    DATA(score) = 0.
    LOOP AT lane REFERENCE INTO DATA(lane_item).
      score = score + 1.
      IF tree->height( ) <= lane_item->height.
        result = score.
        RETURN.
      ENDIF.
    ENDLOOP.
    result = score.
  ENDMETHOD.

ENDCLASS.

CLASS input_processor DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING input TYPE stringtab.
    METHODS get_trees RETURNING VALUE(result) TYPE REF TO trees.

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
    trees->build_forest( ).
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

CLASS application DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING input TYPE stringtab.
    METHODS main.
    METHODS check_invisibility
      RETURNING
        VALUE(result) TYPE i.
    METHODS check_visibility
      RETURNING
        VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA input TYPE stringtab.
    DATA input_processor TYPE REF TO input_processor.
    DATA trees TYPE REF TO trees.
    DATA forest TYPE REF TO forest.
    DATA surveyor TYPE REF TO surveyor.

ENDCLASS.

CLASS application IMPLEMENTATION.

  METHOD constructor.
    me->input = input.
  ENDMETHOD.

  METHOD main.
    input_processor = NEW #( input ).
    trees = input_processor->get_trees( ).
    forest = NEW forest( trees ).
    forest->build( ).
    surveyor = NEW #( forest ).
  ENDMETHOD.

  METHOD check_invisibility.
    surveyor->check_invisibility( trees ).
    result = trees->get_visible_trees( ).
  ENDMETHOD.

  METHOD check_visibility.
    trees->reset_current_tree( ).
    surveyor->check_visibility( trees ).
    result = trees->get_highest_scenic_score( ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_tree DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO tree.
    METHODS create_tree_object FOR TESTING.
    METHODS get_height_of_tree FOR TESTING.

ENDCLASS.

CLASS tc_tree IMPLEMENTATION.

  METHOD create_tree_object.
    cut = NEW #( 2 ).
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD get_height_of_tree.
    cut = NEW #( 4 ).
    cl_abap_unit_assert=>assert_equals( exp = 4 act = cut->height( ) ).
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
    METHODS get_all_trees               FOR TESTING.

ENDCLASS.

CLASS tc_tree_collection IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    cut->add( VALUE #( x_pos = 1 y_pos = 1 height = 3 ) ).
    cut->add( VALUE #( x_pos = 1 y_pos = 2 height = 4 ) ).
  ENDMETHOD.

  METHOD create_object.
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD store_tree_with_coordinates.
    DATA(tree) = cut->get_tree_by_coordinates( x = 1 y = 1 ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = tree->height( )  ).
  ENDMETHOD.

  METHOD request_a_non_existing_tree.
    DATA(tree) = cut->get_tree_by_coordinates( x = 6 y = 1 ).
    cl_abap_unit_assert=>assert_not_bound( act = tree ).
  ENDMETHOD.

  METHOD get_all_trees.
    DATA(count) = 0.
    WHILE cut->has_next( ).
      DATA(tree_item) = cut->get_next( ).
      count = count + 1.
    ENDWHILE.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = count ).
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

CLASS tc_forest DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO forest.

    METHODS setup.

    METHODS get_trees_southward FOR TESTING.
    METHODS get_trees_northward FOR TESTING.
    METHODS get_trees_eastward FOR TESTING.
    METHODS get_trees_westward FOR TESTING.

ENDCLASS.

CLASS tc_forest IMPLEMENTATION.

  METHOD setup.
    DATA(input) = VALUE stringtab( ( |30373| )
                                   ( |25512| )
                                   ( |65332| )
                                   ( |33549| )
                                   ( |35390| ) ) .
    DATA(input_processor) = NEW input_processor( input ).
    DATA(trees) = input_processor->get_trees( ).
    cut = NEW #( trees ).
    cut->build( ).
  ENDMETHOD.

  METHOD get_trees_northward.
    DATA(expected_values) = VALUE if_trees=>tree_properties_tab( ( x_pos = 3 y_pos = 2 height = 5 )
                                                                 ( x_pos = 3 y_pos = 1 height = 3 ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->northward_trees( VALUE #( x_pos = 3 y_pos = 3 ) ) ).
  ENDMETHOD.

  METHOD get_trees_eastward.
    DATA(expected_values) = VALUE if_trees=>tree_properties_tab( ( x_pos = 4 y_pos = 3 height = 3 )
                                                                 ( x_pos = 5 y_pos = 3 height = 2  ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->eastward_trees( VALUE #( x_pos = 3 y_pos = 3 ) ) ).
  ENDMETHOD.

  METHOD get_trees_southward.
    DATA(expected_values) = VALUE if_trees=>tree_properties_tab( ( x_pos = 3 y_pos = 4 height = 5 )
                                                                 ( x_pos = 3 y_pos = 5 height = 3  ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->southward_trees( VALUE #( x_pos = 3 y_pos = 3 ) ) ).
  ENDMETHOD.

  METHOD get_trees_westward.
    DATA(expected_values) = VALUE if_trees=>tree_properties_tab( ( x_pos = 2 y_pos = 3 height = 5 )
                                                                 ( x_pos = 1 y_pos = 3 height = 6  ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->westward_trees( VALUE #( x_pos = 3 y_pos = 3 ) ) ).
  ENDMETHOD.
ENDCLASS.

CLASS tc_visibility DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO surveyor.
    DATA trees TYPE REF TO trees.

    METHODS setup.
    METHODS check_invisibility_of_a_tree FOR TESTING.
    METHODS check_visibility_of_a_tree   FOR TESTING.

ENDCLASS.

CLASS tc_visibility IMPLEMENTATION.

  METHOD setup.
    DATA(input) = VALUE stringtab( ( |30373| )
                                   ( |25512| )
                                   ( |65332| )
                                   ( |33549| )
                                   ( |35390| ) ) .
    DATA(input_processor) = NEW input_processor( input ).
    trees = input_processor->get_trees( ).
    DATA(forest) = NEW forest( trees ).
    forest->build( ).
    cut = NEW #( forest ).
  ENDMETHOD.

  METHOD check_invisibility_of_a_tree.
    cut->check_invisibility( trees ).
    cl_abap_unit_assert=>assert_equals( exp = 21 act = trees->get_visible_trees( )  ).
  ENDMETHOD.

  METHOD check_visibility_of_a_tree.
    cut->check_visibility( trees ).
    cl_abap_unit_assert=>assert_equals( exp = 8 act = trees->get_highest_scenic_score( ) ).
  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.
  DATA(input) = NEW input_reader( )->read_file_in_table( ).

  DATA(application) = NEW application( input ).
  application->main( ).

  WRITE / |Solution part 1: { application->check_invisibility( ) }|.
  WRITE / |Solution part 2: { application->check_visibility( ) }|.
