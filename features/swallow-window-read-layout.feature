Feature: sw/read-layout
  Used while developing the test helpers. That code is frightening.

  Scenario: one window
    When I read the window layout:
      """
      +---+
      | A |
      +---+
      """
    Then the layout should be (rows "A")

  Scenario: A|B
    When I read the window layout:
      """
      +---+---+
      | A | B |
      +---+---+
      """
    Then the layout should be (rows (cols "A" "B"))

  Scenario: A|B--C
    When I read the window layout:
      """
      +---+---+
      | A | B |
      +---+---+
      |   C   |
      +-------+
      """
    Then the layout should be (rows (cols "A" "B") "C")

  Scenario: A|B--C|D
    When I read the window layout:
      """
      +---+---+
      | A | B |
      +---+---+
      | C | D |
      +-------+
      """
    Then the layout should be (rows (cols "A" "B") (cols "C" "D"))

  Scenario: 3 rows
    When I read the window layout:
      """
      +---+
      | A |
      +---+
      | B |
      +---+
      | C |
      +---+
      """
    Then the layout should be (rows "A" "B" "C")

  Scenario: Inner horizontal split
    When I read the window layout:
      """
      +---+---+
      | A |   |
      +---+ B |
      | F |   |
      +---+---+
      |   C   |
      +-------+
      | D | E |
      +---+---+
      """
    Then the layout should be (rows (cols (rows "A" "F") "B") "C" (cols "D" "E"))

  Scenario: Inner vertical split
    When I read the window layout:
      """
      +---+---+---+
      |   | B | C |
      | A +---+---+
      |   |   D   |
      +---+---+---+
      |     E     |
      +-----------+
      """
    Then the layout should be (rows (cols "A" (rows (cols "B" "C") "D")) "E")
