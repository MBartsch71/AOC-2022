REPORT ymbh_aoc_2022_day_06.

CLASS input_reader DEFINITION.
  PUBLIC SECTION.
    METHODS read_file_in_table RETURNING VALUE(result) TYPE stringtab.
ENDCLASS.

CLASS input_reader IMPLEMENTATION.

  METHOD read_file_in_table.
    DATA(file_reader) = NEW zcl_mbh_file_upload( |/Users/mbartsch71/github/AOC-2022/inputs/20221206| ).
    result = file_reader->file_upload_in_stringtab( ).
  ENDMETHOD.

ENDCLASS.

CLASS marker_locator DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING input TYPE string.

    METHODS detect_marker_position IMPORTING sequence_length TYPE i
                                   RETURNING VALUE(result)   TYPE i.

  PRIVATE SECTION.
    DATA input TYPE string.
    DATA resetted TYPE abap_bool.
    DATA restart_offset TYPE i.

    METHODS sequence_is_too_short IMPORTING sequence      TYPE string
                                            length        TYPE i
                                  RETURNING VALUE(result) TYPE abap_bool.

    METHODS get_character IMPORTING input         TYPE string
                                    offset        TYPE i
                          RETURNING VALUE(result) TYPE text1.

    METHODS add_unique_letter IMPORTING sequence      TYPE string
                                        letter        TYPE text1
                              RETURNING VALUE(result) TYPE string.

    METHODS reset_sequence IMPORTING letter        TYPE text1
                           RETURNING VALUE(result) TYPE string.

    METHODS sequence_is_resetted RETURNING VALUE(result) TYPE abap_bool.

    METHODS restart_sequence RETURNING VALUE(result) TYPE i.

    METHODS get_new_offset IMPORTING offset        TYPE i
                           RETURNING VALUE(result) TYPE i.

    METHODS increase_offset IMPORTING offset        TYPE i
                            RETURNING VALUE(result) TYPE i.
ENDCLASS.

CLASS marker_locator IMPLEMENTATION.

  METHOD constructor.
    me->input = input.
  ENDMETHOD.

  METHOD detect_marker_position.
    DATA(sequence) = ||.
    DATA(offset) = 0.

    WHILE sequence_is_too_short( sequence = sequence
                                 length = sequence_length ).

      DATA(letter) = get_character( input = input offset = offset ).
      sequence = add_unique_letter( sequence = sequence letter = letter ).
      offset = get_new_offset( offset ).
    ENDWHILE.

    result = offset.
  ENDMETHOD.

  METHOD sequence_is_too_short.
    result = xsdbool( strlen( sequence ) < length ).
  ENDMETHOD.

  METHOD get_character.
    result = substring( val = input off = offset len = 1 ).
  ENDMETHOD.

  METHOD add_unique_letter.
    result = COND #( WHEN sequence NS letter THEN |{ sequence }{ letter }|
                     ELSE reset_sequence( letter ) ).
  ENDMETHOD.

  METHOD reset_sequence.
    resetted = abap_true.
  ENDMETHOD.

  METHOD get_new_offset.
    result = COND #( WHEN sequence_is_resetted( )
                        THEN restart_sequence( )
                        ELSE increase_offset( offset = offset ) ).
  ENDMETHOD.

  METHOD sequence_is_resetted.
    result = xsdbool( resetted = abap_true ).
  ENDMETHOD.

  METHOD restart_sequence.
    resetted = abap_false.
    restart_offset = restart_offset + 1.
    result = restart_offset.
  ENDMETHOD.

  METHOD increase_offset.
    result = offset + 1.
  ENDMETHOD.

ENDCLASS.

CLASS tc_sequence_counter DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS start_of_packet_marker TYPE i VALUE 4.
    CONSTANTS start_of_message_marker TYPE i VALUE 14.
    DATA cut TYPE REF TO marker_locator.

    METHODS get_position_of_marker_1 FOR TESTING.
    METHODS get_position_of_marker_2 FOR TESTING.
    METHODS get_position_of_marker_3 FOR TESTING.

    METHODS get_message_marker_1 FOR TESTING.

ENDCLASS.

CLASS tc_sequence_counter IMPLEMENTATION.

  METHOD get_position_of_marker_1.
    cut = NEW #( |mjqjpqmgbljsphdztnvjfqwrcgsmlb| ).
    cl_abap_unit_assert=>assert_equals( exp = 7 act = cut->detect_marker_position( start_of_packet_marker ) ).
  ENDMETHOD.

  METHOD get_position_of_marker_2.
    cut = NEW #( |bvwbjplbgvbhsrlpgdmjqwftvncz| ).
    cl_abap_unit_assert=>assert_equals( exp = 5 act = cut->detect_marker_position( start_of_packet_marker ) ).
  ENDMETHOD.

  METHOD get_position_of_marker_3.
    cut = NEW #( |nppdvjthqldpwncqszvftbrmjlhg| ).
    cl_abap_unit_assert=>assert_equals( exp = 6 act = cut->detect_marker_position( start_of_packet_marker ) ).
  ENDMETHOD.

  METHOD get_message_marker_1.
    cut = NEW #( |mjqjpqmgbljsphdztnvjfqwrcgsmlb| ).
    cl_abap_unit_assert=>assert_equals( exp = 19 act = cut->detect_marker_position( start_of_message_marker ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input) = NEW input_reader( )->read_file_in_table( ).

  DATA(marker_locator) = NEW marker_locator( input[ 1 ] ).
  WRITE / |Solution part 1: { marker_locator->detect_marker_position( 4 ) }|.
  WRITE / |Solution part 2: { marker_locator->detect_marker_position( 14 ) }|.
