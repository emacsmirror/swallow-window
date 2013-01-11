Feature: swallow-window

  Background:
    Given I delete all other windows

  Scenario Outline: From sole window
    When I swallow-window <dir>
    Then I should see message "swallow-window: no other window to swallow"

    Examples:
      | dir   |
      | up    |
      | down  |
      | left  |
      | right |


  Scenario Outline: Swallow the only other window to the side
    Given the window layout:
      """
      +---+---+
      | A | B |
      +---+---+
      """
    When I select window <window>
    And  I swallow-window <dir>
    Then window <window> should be the only window in the frame

    Examples:
      | window | dir   |
      | A      | right |
      | B      | left  |


  Scenario Outline: Swallow the only other window above or below
    Given the window layout:
      """
      +---+
      | A |
      +---+
      | B |
      +---+
      """
    When I select window <window>
    And  I swallow-window <dir>
    Then window <window> should be the only window in the frame

    Examples:
      | window | dir  |
      | A      | down |
      | B      | up   |


  Scenario Outline: 2x2 windows
    Given the window layout:
      """
      +---+---+
      | A | B |
      +---+---+
      | C | D |
      +---+---+
      """
    When I select window <window>
    And  I swallow-window <dir>
    Then window <swallowed> should be deleted
    And  window <window> should be the only window on the <side>

    Examples:
      | window | dir   | swallowed | side   |
      | A      | down  | C         | left   |
      | A      | right | B         | top    |
      | B      | down  | D         | right  |
      | B      | left  | A         | top    |
      | C      | up    | A         | left   |
      | C      | right | D         | bottom |
      | D      | up    | B         | right  |
      | D      | left  | C         | bottom |
