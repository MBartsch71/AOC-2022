REPORT ymbh_aoc_2022_day_03.



CLASS input_reader DEFINITION.
  PUBLIC SECTION.
    METHODS read_file_in_table RETURNING VALUE(result) TYPE stringtab.
ENDCLASS.

CLASS input_reader IMPLEMENTATION.

  METHOD read_file_in_table.
    DATA(file_reader) = NEW zcl_mbh_file_upload( |/Users/mbartsch71/github/AOC-2022/inputs/20221203 | ).
    result = file_reader->file_upload_in_stringtab( ).
  ENDMETHOD.

ENDCLASS.

CLASS priority DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.

    METHODS get_priority_for IMPORTING item          TYPE text1
                             RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    TYPES: BEGIN OF item_priority,
             item_letter TYPE text1,
             priority    TYPE i,
           END OF item_priority.
    TYPES item_priorities TYPE SORTED TABLE OF item_priority WITH NON-UNIQUE KEY primary_key COMPONENTS priority.
    DATA items TYPE item_priorities.

ENDCLASS.

CLASS priority IMPLEMENTATION.

  METHOD constructor.
    items = VALUE #( ( item_letter = |a| priority = 1 )  ( item_letter = |b| priority = 2 )
                     ( item_letter = |c| priority = 3 )  ( item_letter = |d| priority = 4 )
                     ( item_letter = |e| priority = 5 )  ( item_letter = |f| priority = 6 )
                     ( item_letter = |g| priority = 7 )  ( item_letter = |h| priority = 8 )
                     ( item_letter = |i| priority = 9 )  ( item_letter = |j| priority = 10 )
                     ( item_letter = |k| priority = 11 ) ( item_letter = |l| priority = 12 )
                     ( item_letter = |m| priority = 13 ) ( item_letter = |n| priority = 14 )
                     ( item_letter = |o| priority = 15 ) ( item_letter = |p| priority = 16 )
                     ( item_letter = |q| priority = 17 ) ( item_letter = |r| priority = 18 )
                     ( item_letter = |s| priority = 19 ) ( item_letter = |t| priority = 20 )
                     ( item_letter = |u| priority = 21 ) ( item_letter = |v| priority = 22 )
                     ( item_letter = |w| priority = 23 ) ( item_letter = |x| priority = 24 )
                     ( item_letter = |y| priority = 25 ) ( item_letter = |z| priority = 26 )
                     ( item_letter = |A| priority = 27 ) ( item_letter = |B| priority = 28 )
                     ( item_letter = |C| priority = 29 ) ( item_letter = |D| priority = 30 )
                     ( item_letter = |E| priority = 31 ) ( item_letter = |F| priority = 32 )
                     ( item_letter = |G| priority = 33 ) ( item_letter = |H| priority = 34 )
                     ( item_letter = |I| priority = 35 ) ( item_letter = |J| priority = 36 )
                     ( item_letter = |K| priority = 37 ) ( item_letter = |L| priority = 38 )
                     ( item_letter = |M| priority = 39 ) ( item_letter = |N| priority = 40 )
                     ( item_letter = |O| priority = 41 ) ( item_letter = |P| priority = 42 )
                     ( item_letter = |Q| priority = 43 ) ( item_letter = |R| priority = 44 )
                     ( item_letter = |S| priority = 45 ) ( item_letter = |T| priority = 46 )
                     ( item_letter = |U| priority = 47 ) ( item_letter = |V| priority = 48 )
                     ( item_letter = |W| priority = 49 ) ( item_letter = |X| priority = 50 )
                     ( item_letter = |Y| priority = 51 ) ( item_letter = |Z| priority = 52 ) ).
  ENDMETHOD.

  METHOD get_priority_for.
    result = items[ item_letter = item ]-priority.
  ENDMETHOD.

ENDCLASS.

CLASS string_tool DEFINITION.
  PUBLIC SECTION.
    METHODS halve IMPORTING input         TYPE string
                  RETURNING VALUE(result) TYPE stringtab.

    METHODS find_duplicate IMPORTING input         TYPE stringtab
                           RETURNING VALUE(result) TYPE text1.

    METHODS find_triples IMPORTING input         TYPE stringtab
                         RETURNING VALUE(result) TYPE text1.
ENDCLASS.

CLASS string_tool IMPLEMENTATION.

  METHOD halve.
    IF strlen( input ) MOD 2 = 0.

      DATA(split_pos) = strlen( input ) / 2.
      result = VALUE #( ( substring( val = input off = 0 len = split_pos ) )
                        ( substring( val = input off = split_pos len = split_pos ) ) ).
    ENDIF.
  ENDMETHOD.

  METHOD find_duplicate.
    DATA offset TYPE i.

    DO.
      DATA(letter) = substring( val = input[ 1 ] off = offset len = 1 ).
      FIND FIRST OCCURRENCE OF letter IN input[ 2 ] RESPECTING CASE.
      IF sy-subrc = 0.
        result = letter.
        RETURN.
      ENDIF.
      offset = offset + 1.
      IF offset > strlen( input[ 1 ] ).
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD find_triples.
    DATA offset TYPE i.

    DO.
      DATA(letter) = substring( val = input[ 1 ] off = offset len = 1 ).
      FIND FIRST OCCURRENCE OF letter IN input[ 2 ] RESPECTING CASE.
      IF sy-subrc = 0.
        FIND FIRST OCCURRENCE OF letter IN input[ 3 ] RESPECTING CASE.
        IF sy-subrc = 0.

          result = letter.
          RETURN.
        ENDIF.
      ENDIF.
      offset = offset + 1.
      IF offset > strlen( input[ 1 ] ).
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.

ENDCLASS.

CLASS text_parser DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF stringgroup,
             group TYPE stringtab,
           END OF stringgroup.
    TYPES stringgroups TYPE STANDARD TABLE OF stringgroup WITH EMPTY KEY.

    METHODS constructor IMPORTING text TYPE stringtab.

    METHODS find_doubles_in_lines RETURNING VALUE(result) TYPE stringtab.

    METHODS get_priority_sum IMPORTING input         TYPE stringtab
                             RETURNING VALUE(result) TYPE i.

    METHODS find_triples_in_lines RETURNING VALUE(result) TYPE stringtab.

  PRIVATE SECTION.
    DATA content TYPE stringtab.
    DATA groups TYPE stringgroups.
    DATA stringtool TYPE REF TO string_tool.
    DATA prioritytool TYPE REF TO priority.

    METHODS build_groups_of_three RETURNING VALUE(result) TYPE stringgroups.

ENDCLASS.

CLASS text_parser IMPLEMENTATION.

  METHOD constructor.
    content = text.
    groups = build_groups_of_three( ).
    stringtool = NEW #( ).
    prioritytool = NEW #( ).
  ENDMETHOD.

  METHOD find_doubles_in_lines.
    LOOP AT content REFERENCE INTO DATA(content_line).
      DATA(substrings) = stringtool->halve( content_line->* ).
      DATA(double_letter) = stringtool->find_duplicate( substrings ).
      APPEND double_letter TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_priority_sum.
    result = REDUCE #( INIT sum = 0
                       FOR line IN input
                       NEXT sum = sum + prioritytool->get_priority_for( CONV #( line ) ) ).
  ENDMETHOD.


  METHOD build_groups_of_three.
    DATA(line_end) = lines( content ).
    DATA start_line TYPE i VALUE 1.
    DATA part TYPE stringtab.
    DO.
      part = VALUE #( FOR i = start_line THEN i + 1 WHILE i <= start_line + 2
                                      ( content[ i ] ) ).
      result = VALUE #( BASE result ( group = part ) ).
      CLEAR part.
      start_line = start_line + 3.
      IF start_line >= line_end.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD find_triples_in_lines.
    LOOP AT groups REFERENCE INTO DATA(group_line).
      DATA(letter) = stringtool->find_triples( group_line->group ).
      APPEND letter TO result.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS tc_item_priotities DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO priority.

    METHODS setup.

    METHODS find_priority_lowercase_letter FOR TESTING.
    METHODS find_priority_uppercase_letter FOR TESTING.

ENDCLASS.

CLASS tc_item_priotities IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD find_priority_lowercase_letter.
    cl_abap_unit_assert=>assert_equals( exp = 16 act = cut->get_priority_for( |p| ) ).
  ENDMETHOD.

  METHOD find_priority_uppercase_letter.
    cl_abap_unit_assert=>assert_equals( exp = 38 act = cut->get_priority_for( |L| ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_string_split DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO string_tool.

    METHODS setup.
    METHODS split_string_in_half FOR TESTING.

    METHODS find_double_lowercase_letter FOR TESTING.
    METHODS find_double_uppercase_letter FOR TESTING.

    METHODS find_triple_lowercase_letter FOR TESTING.
    METHODS find_triple_uppercase_letter FOR TESTING.

ENDCLASS.

CLASS tc_string_split IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD split_string_in_half.
    DATA(expected_values) = VALUE stringtab( ( |vJrwpWtwJgWr| )
                                             ( |hcsFMMfFFhFp| ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->halve( |vJrwpWtwJgWrhcsFMMfFFhFp| ) ).
  ENDMETHOD.

  METHOD find_double_lowercase_letter.
    DATA(input) = VALUE stringtab( ( |vJrwpWtwJgWr| )
                                   ( |hcsFMMfFFhFp| ) ).
    cl_abap_unit_assert=>assert_equals( exp = |p| act = cut->find_duplicate( input )  ).
  ENDMETHOD.

  METHOD find_double_uppercase_letter.
    DATA(input) = VALUE stringtab( ( |jqHRNqRjqzjGDLGL| )
                                   ( |rsFMfFZSrLrFZsSL| ) ).
    cl_abap_unit_assert=>assert_equals( exp = |L| act = cut->find_duplicate( input )  ).
  ENDMETHOD.

  METHOD find_triple_lowercase_letter.
    DATA(input) = VALUE stringtab( ( |vJrwpWtwJgWrhcsFMMfFFhFp| )
                                   ( |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL| )
                                   ( |PmmdzqPrVvPwwTWBwg| ) ).
    cl_abap_unit_assert=>assert_equals( exp = |r| act = cut->find_triples( input ) ).
  ENDMETHOD.

  METHOD find_triple_uppercase_letter.
    DATA(input) = VALUE stringtab( ( |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn| )
                                   ( |ttgJtRGJQctTZtZT| )
                                   ( |CrZsJsPPZsGzwwsLwLmpwMDw| ) ).
    cl_abap_unit_assert=>assert_equals( exp = |Z| act = cut->find_triples( input ) ).

  ENDMETHOD.

ENDCLASS.

CLASS tc_string_parser DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO text_parser.

    METHODS setup.
    METHODS parse_text_for_dupliates FOR TESTING.
    METHODS get_priority_sum_of_duplicates FOR TESTING.


    METHODS parse_text_for_triples FOR TESTING.
    METHODS get_priority_sum_of_triplets FOR TESTING.

ENDCLASS.

CLASS tc_string_parser IMPLEMENTATION.

  METHOD setup.
    DATA(input) = VALUE stringtab( ( |vJrwpWtwJgWrhcsFMMfFFhFp| )
                                   ( |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL| )
                                   ( |PmmdzqPrVvPwwTWBwg| )
                                   ( |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn| )
                                   ( |ttgJtRGJQctTZtZT| )
                                   ( |CrZsJsPPZsGzwwsLwLmpwMDw| ) ).
    cut = NEW #( input ).
  ENDMETHOD.

  METHOD parse_text_for_dupliates.
    DATA(expected_values) = VALUE stringtab( ( |p| )
                                             ( |L| )
                                             ( |P| )
                                             ( |v| )
                                             ( |t| )
                                             ( |s| ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->find_doubles_in_lines( )  ).
  ENDMETHOD.

  METHOD get_priority_sum_of_duplicates.
    DATA(input) = cut->find_doubles_in_lines( ).
    cl_abap_unit_assert=>assert_equals( exp = 157 act = cut->get_priority_sum( input )  ).
  ENDMETHOD.

  METHOD get_priority_sum_of_triplets.
    DATA(input) = cut->find_triples_in_lines( ).
    cl_abap_unit_assert=>assert_equals( exp = 70 act = cut->get_priority_sum( input ) ).
  ENDMETHOD.

  METHOD parse_text_for_triples.
    DATA(expected_values) = VALUE stringtab( ( |r| )
                                             ( |Z| ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->find_triples_in_lines( ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input) = NEW input_reader( )->read_file_in_table( ).

  DATA(text) = NEW text_parser( input ).
  DATA(doubles) = text->find_doubles_in_lines( ).
  WRITE / |Solution first part: { text->get_priority_sum( doubles ) }|.

  DATA(triples) = text->find_triples_in_lines( ).
  WRITE / |Solution second part: { text->get_priority_sum( triples ) }|.
