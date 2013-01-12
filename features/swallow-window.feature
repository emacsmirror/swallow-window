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

  Scenario Outline: 3 stack
    Given the window layout:
      """
      +---+
      | A |
      +---+
      | B |
      +---+
      | C |
      +---+
      """
    When I select window <window>
    And  I swallow-window <dir1>
    Then window <swallowed1> should be deleted
    And  window <swallowed2> should be the same size
    When I swallow-window <dir2>
    Then window <swallowed2> should be deleted
    And  window <window> should be the only window in the frame

    Examples:
      | window | dir1 | swallowed1 | dir2 | swallowed2 |
      | A      | down | B          | down | C          |
      | B      | up   | A          | down | C          |
      | B      | down | C          | up   | A          |
      | C      | up   | B          | up   | A          |

  Scenario Outline: Consume two windows at once
    Given the window layout:
      """
      +---+---+
      | A | B |
      +-------+
      |   C   |
      +-------+
      | D | E |
      +---+---+
      """
    When I select window <window>
    And  I swallow-window <dir>
    Then window <swallowed1> should be deleted
    And  window <swallowed2> should be deleted

    Examples:
      | window | dir  | swallowed1 | swallowed2 |
      | C      | up   | A          | B          |
      | C      | down | D          | E          |

  Scenario Outline: Downsizing a window
    Given the window layout:
      """
      +---+---+
      | A |   |
      +---+ C |
      | B |   |
      +---+---+
      """
    When I select window <window>
    And  I swallow-window <dir>
    Then window <window> should be the full frame width
    And  window <resized> should be <relation> window <window>

    Examples:
      | window | dir   | resized | relation |
      | A      | right | C       | below    |
      | B      | right | C       | above    |
