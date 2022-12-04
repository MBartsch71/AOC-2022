REPORT ymbh_aoc_2022_day_04.

CLASS input_reader DEFINITION.
  PUBLIC SECTION.
    METHODS read_file_in_table RETURNING VALUE(result) TYPE stringtab.
ENDCLASS.

CLASS input_reader IMPLEMENTATION.

  METHOD read_file_in_table.
    DATA(file_reader) = NEW zcl_mbh_file_upload( |/Users/mbartsch71/github/AOC-2022/inputs/20221204 | ).
    result = file_reader->file_upload_in_stringtab( ).
  ENDMETHOD.

ENDCLASS.

CLASS set DEFINITION.

  PUBLIC SECTION.
    METHODS constructor IMPORTING input TYPE string.

    METHODS lower_bound RETURNING VALUE(result) TYPE i.

    METHODS upper_bound RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    TYPES set_elements TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY primary_key COMPONENTS table_line.
    DATA elements TYPE set_elements.

    METHODS resolve_set IMPORTING input TYPE string.

ENDCLASS.

CLASS set IMPLEMENTATION.

  METHOD constructor.
    resolve_set( input ).
  ENDMETHOD.

  METHOD lower_bound.
    result = elements[ 1 ].
  ENDMETHOD.

  METHOD upper_bound.
    result = elements[ lines( elements ) ].
  ENDMETHOD.

  METHOD resolve_set.
    SPLIT input AT '-' INTO DATA(lower_bound) DATA(upper_bound).
    elements = VALUE #( FOR i = CONV i( lower_bound ) THEN i + 1 WHILE i <= upper_bound
                              ( i ) ).
  ENDMETHOD.

ENDCLASS.

CLASS set_comparator DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING set1 TYPE REF TO set
                                  set2 TYPE REF TO set.

    METHODS one_set_is_fully_contained RETURNING VALUE(result) TYPE abap_bool.

    METHODS one_set_is_partially_contained RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    DATA set1 TYPE REF TO set.
    DATA set2 TYPE REF TO set.

    METHODS set1_fully_in_set2 RETURNING VALUE(result) TYPE abap_bool.
    METHODS set2_fully_in_set1 RETURNING VALUE(result) TYPE abap_bool.

    METHODS set1_lower_bound_in_set2 RETURNING VALUE(result) TYPE abap_bool.
    METHODS set1_upper_bound_in_set2 RETURNING VALUE(result) TYPE abap_bool.

    METHODS set2_lower_bound_in_set1 RETURNING VALUE(result) TYPE abap_bool.
    METHODS set2_upper_bound_in_set1 RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.

CLASS set_comparator IMPLEMENTATION.

  METHOD constructor.
    me->set1 = set1.
    me->set2 = set2.
  ENDMETHOD.

  METHOD one_set_is_fully_contained.
    result = xsdbool( set1_fully_in_set2( ) OR
                      set2_fully_in_set1( ) ).
  ENDMETHOD.

  METHOD one_set_is_partially_contained.
    result = xsdbool( set1_lower_bound_in_set2( ) OR
                      set1_upper_bound_in_set2( ) OR
                      set2_lower_bound_in_set1( ) OR
                      set2_upper_bound_in_set1( ) ).
  ENDMETHOD.

  METHOD set1_fully_in_set2.
    result = xsdbool( set1->lower_bound( ) >= set2->lower_bound( ) AND
                      set1->upper_bound( ) <= set2->upper_bound( ) ).
  ENDMETHOD.

  METHOD set2_fully_in_set1.
    result = xsdbool( set2->lower_bound( ) >= set1->lower_bound( ) AND
                      set2->upper_bound( ) <= set1->upper_bound( ) ).
  ENDMETHOD.

  METHOD set1_lower_bound_in_set2.
    result = xsdbool( set1->lower_bound( ) >= set2->lower_bound( ) AND
                      set1->lower_bound( ) <= set2->upper_bound( ) ).
  ENDMETHOD.

  METHOD set1_upper_bound_in_set2.
    result = xsdbool( set1->upper_bound( ) >= set2->lower_bound( ) AND
                      set1->upper_bound( ) <= set2->upper_bound( ) ).
  ENDMETHOD.

  METHOD set2_lower_bound_in_set1.
    result = xsdbool( set2->lower_bound( ) >= set1->lower_bound( ) AND
                      set2->lower_bound( ) <= set1->upper_bound( ) ).
  ENDMETHOD.

  METHOD set2_upper_bound_in_set1.
    result = xsdbool( set2->upper_bound( ) >= set1->lower_bound( ) AND
                      set2->upper_bound( ) <= set1->upper_bound( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS set_list DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS fully_overlapping_sets_count IMPORTING input         TYPE stringtab
                                         RETURNING VALUE(result) TYPE i.

    METHODS part_overlapping_sets_count IMPORTING input         TYPE stringtab
                                        RETURNING VALUE(result) TYPE i.
  PRIVATE SECTION.
    METHODS compare_from_line IMPORTING line          TYPE string
                              RETURNING VALUE(result) TYPE REF TO set_comparator.

ENDCLASS.

CLASS set_list IMPLEMENTATION.

  METHOD fully_overlapping_sets_count.
    result = REDUCE #( INIT sum = 0
                       FOR line IN input
                       NEXT sum = COND #( WHEN compare_from_line( line )->one_set_is_fully_contained( ) THEN sum + 1
                                          ELSE sum ) ).
  ENDMETHOD.

  METHOD part_overlapping_sets_count.
    result = REDUCE #( INIT sum = 0
                       FOR line IN input
                       NEXT sum = COND #( WHEN compare_from_line( line )->one_set_is_partially_contained( ) THEN sum + 1
                                          ELSE sum ) ).
  ENDMETHOD.

  METHOD compare_from_line.
    SPLIT line AT ',' INTO DATA(first_set) DATA(second_set).
    result = NEW #( set1 = NEW #( first_set )
                    set2 = NEW #( second_set ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_elements DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO set.

    METHODS setup.
    METHODS get_lower_bound_of_set FOR TESTING.
    METHODS get_upper_bound_of_set FOR TESTING.

ENDCLASS.

CLASS tc_elements IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( |2-4| ).
  ENDMETHOD.

  METHOD get_lower_bound_of_set.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = cut->lower_bound( )  ).
  ENDMETHOD.

  METHOD get_upper_bound_of_set.
    cl_abap_unit_assert=>assert_equals( exp = 4 act = cut->upper_bound( )  ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_set_comparator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO set_comparator.

    METHODS set_1_is_fully_in_set_2 FOR TESTING.
    METHODS set_2_is_fully_in_set_1 FOR TESTING.

    METHODS set_1_is_partially_in_set_2 FOR TESTING.
    METHODS set_2_is_partially_in_set_1 FOR TESTING.

ENDCLASS.

CLASS tc_set_comparator IMPLEMENTATION.

  METHOD set_1_is_fully_in_set_2.
    cut = NEW #( set1 = NEW #( |6-6| ) set2 = NEW #( |4-6| ) ).
    cl_abap_unit_assert=>assert_true( act = cut->one_set_is_fully_contained( ) ).
  ENDMETHOD.

  METHOD set_2_is_fully_in_set_1.
    cut = NEW #( set1 = NEW #( |2-8| ) set2 = NEW #( |3-7| ) ).
    cl_abap_unit_assert=>assert_true( act = cut->one_set_is_fully_contained( ) ).
  ENDMETHOD.

  METHOD set_1_is_partially_in_set_2.
    cut = NEW #( set1 = NEW #( |5-7| ) set2 = NEW #( |7-9| ) ).
    cl_abap_unit_assert=>assert_true( act = cut->one_set_is_partially_contained( ) ).
  ENDMETHOD.

  METHOD set_2_is_partially_in_set_1.
    cut = NEW #( set1 = NEW #( |2-8| ) set2 = NEW #( |3-7| ) ).
    cl_abap_unit_assert=>assert_true( act = cut->one_set_is_partially_contained( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_set_list DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO set_list.
    DATA input_list TYPE stringtab.

    METHODS setup.
    METHODS fully_overlapping_sets_count FOR TESTING.
    METHODS partially_overlapping_sets FOR TESTING.
ENDCLASS.

CLASS tc_set_list IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    input_list = VALUE #( ( |2-4,6-8| )
                          ( |2-3,4-5| )
                          ( |5-7,7-9| )
                          ( |2-8,3-7| )
                          ( |6-6,4-6| )
                          ( |2-6,4-8| ) ).
  ENDMETHOD.

  METHOD fully_overlapping_sets_count.
    cl_abap_unit_assert=>assert_equals( exp = 2 act = cut->fully_overlapping_sets_count( input_list )  ).
  ENDMETHOD.

  METHOD partially_overlapping_sets.
    cl_abap_unit_assert=>assert_equals( exp = 4 act = cut->part_overlapping_sets_count( input_list )  ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input) = NEW input_reader( )->read_file_in_table( ).

  DATA(set_list) = NEW set_list( ).
  WRITE / |Solution part 1: { set_list->fully_overlapping_sets_count( input ) }|.

  WRITE / |Solution part 2: { set_list->part_overlapping_sets_count( input ) }|.
