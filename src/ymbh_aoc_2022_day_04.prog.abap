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

  METHOD resolve_set.
    SPLIT input AT '-' INTO DATA(lower_bound) DATA(upper_bound).
    elements = VALUE #( FOR i = CONV i( lower_bound ) THEN i + 1 WHILE i <= upper_bound
                              ( i ) ).
  ENDMETHOD.

  METHOD lower_bound.
    result = elements[ 1 ].
  ENDMETHOD.

  METHOD upper_bound.
    result = elements[ lines( elements ) ].
  ENDMETHOD.

ENDCLASS.

CLASS set_comparator DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS one_set_is_fully_contained IMPORTING set1          TYPE REF TO set
                                                 set2          TYPE REF TO set
                                       RETURNING VALUE(result) TYPE abap_bool.
    METHODS one_set_is_partially_contained
      IMPORTING
        set1          TYPE REF TO set
        set2          TYPE REF TO set
      RETURNING
        VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    METHODS set1_fully_in_set2 IMPORTING set1          TYPE REF TO set
                                         set2          TYPE REF TO set
                               RETURNING VALUE(result) TYPE abap_bool.


    METHODS set2_fully_in_set1 IMPORTING set1          TYPE REF TO set
                                         set2          TYPE REF TO set
                               RETURNING VALUE(result) TYPE abap_bool.


    METHODS set1_lower_bound_in_set2 IMPORTING set1          TYPE REF TO set
                                               set2          TYPE REF TO set
                                     RETURNING VALUE(result) TYPE abap_bool.
    METHODS set1_upper_bound_in_set2 IMPORTING set1          TYPE REF TO set
                                               set2          TYPE REF TO set
                                     RETURNING VALUE(result) TYPE abap_bool.

    METHODS set2_lower_bound_in_set1 IMPORTING set1          TYPE REF TO set
                                               set2          TYPE REF TO set
                                     RETURNING VALUE(result) TYPE abap_bool.
    METHODS set2_upper_bound_in_set1 IMPORTING set1          TYPE REF TO set
                                               set2          TYPE REF TO set
                                     RETURNING VALUE(result) TYPE abap_bool.


ENDCLASS.

CLASS set_comparator IMPLEMENTATION.

  METHOD one_set_is_fully_contained.
    result = set1_fully_in_set2( set1 = set1 set2 = set2 ).
    IF result = abap_true.
      RETURN.
    ENDIF.
    result = set2_fully_in_set1( set1 = set1 set2 = set2 ).
  ENDMETHOD.

  METHOD set1_fully_in_set2.
    result = xsdbool( set1->lower_bound( ) >= set2->lower_bound( ) AND
                      set1->upper_bound( ) <= set2->upper_bound( ) ).
  ENDMETHOD.

  METHOD set2_fully_in_set1.
    result = xsdbool( set2->lower_bound( ) >= set1->lower_bound( ) AND
                      set2->upper_bound( ) <= set1->upper_bound( ) ).
  ENDMETHOD.


  METHOD one_set_is_partially_contained.
    DATA(result1) = set1_lower_bound_in_set2( set1 = set1 set2 = set2 ).
    DATA(result2) = set1_upper_bound_in_set2( set1 = set1 set2 = set2 ).
    DATA(result3) = set2_lower_bound_in_set1( set1 = set1 set2 = set2 ).
    DATA(result4) = set2_upper_bound_in_set1( set1 = set1 set2 = set2 ).
    result = xsdbool( result1 = abap_true OR result2 = abap_true OR result3 = abap_true OR result4 = abap_true ).
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
    METHODS overlapping_set_count IMPORTING input         TYPE stringtab
                                  RETURNING VALUE(result) TYPE i.
    METHODS partially_overlapping_sets
      IMPORTING
        input         TYPE stringtab
      RETURNING
        VALUE(result) TYPE i.

  PRIVATE SECTION.

ENDCLASS.

CLASS set_list IMPLEMENTATION.

  METHOD overlapping_set_count.
    DATA first_set TYPE string.
    DATA second_set TYPE string.
    LOOP AT input REFERENCE INTO DATA(line).
      SPLIT line->* AT ',' INTO first_set second_set.
      DATA(set1) = NEW set( first_set ).
      DATA(set2) = NEW set( second_set ).
      DATA(one_set_is_contained) = NEW set_comparator( )->one_set_is_fully_contained( set1 = set1 set2 = set2 ).
      IF one_set_is_contained =  abap_true.
        result = result + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD partially_overlapping_sets.
    DATA first_set TYPE string.
    DATA second_set TYPE string.
    LOOP AT input REFERENCE INTO DATA(line).
      SPLIT line->* AT ',' INTO first_set second_set.
      DATA(set1) = NEW set( first_set ).
      DATA(set2) = NEW set( second_set ).
      DATA(one_set_is_contained) = NEW set_comparator( )->one_set_is_partially_contained( set1 = set1 set2 = set2 ).
      IF one_set_is_contained =  abap_true.
        result = result + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS tc_elements DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO set.
    TYPES set_elements TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY primary_key COMPONENTS table_line.

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

    METHODS setup.
    METHODS set_1_is_fully_in_set_2 FOR TESTING.
    METHODS set_2_is_fully_in_set_1 FOR TESTING.

    METHODS set_1_is_partially_in_set_2 FOR TESTING.
    METHODS set_2_is_partially_in_set_1 FOR TESTING.
ENDCLASS.

CLASS tc_set_comparator IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD set_1_is_fully_in_set_2.
    DATA(set_1) = NEW set( |6-6| ).
    DATA(set_2) = NEW set( |4-6| ).

    cl_abap_unit_assert=>assert_true( act = cut->one_set_is_fully_contained( set1 = set_1 set2 = set_2 ) ).
  ENDMETHOD.

  METHOD set_2_is_fully_in_set_1.
    DATA(set_1) = NEW set( |2-8| ).
    DATA(set_2) = NEW set( |3-7| ).

    cl_abap_unit_assert=>assert_true( act = cut->one_set_is_fully_contained( set1 = set_1 set2 = set_2 ) ).
  ENDMETHOD.

  METHOD set_1_is_partially_in_set_2.
    DATA(set_1) = NEW set( |5-7| ).
    DATA(set_2) = NEW set( |7-9| ).

    cl_abap_unit_assert=>assert_true( act = cut->one_set_is_partially_contained( set1 = set_1 set2 = set_2 ) ).
  ENDMETHOD.

  METHOD set_2_is_partially_in_set_1.
    DATA(set_1) = NEW set( |2-8| ).
    DATA(set_2) = NEW set( |3-7| ).

    cl_abap_unit_assert=>assert_true( act = cut->one_set_is_partially_contained( set1 = set_1 set2 = set_2 ) ).
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
    cl_abap_unit_assert=>assert_equals( exp = 2 act = cut->overlapping_set_count( input_list )  ).
  ENDMETHOD.

  METHOD partially_overlapping_sets.
    cl_abap_unit_assert=>assert_equals( exp = 4 act = cut->partially_overlapping_sets( input_list )  ).
  ENDMETHOD.

ENDCLASS.



START-OF-SELECTION.
  DATA(input) = NEW input_reader( )->read_file_in_table( ).

  DATA(set_list) = NEW set_list( ).
  WRITE / |Solution part 1: { set_list->overlapping_set_count( input ) }|.

  WRITE / |Solution part 2: { set_list->partially_overlapping_sets( input ) }|.
