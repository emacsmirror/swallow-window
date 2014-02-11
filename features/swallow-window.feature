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


  Scenario Outline: Don't swallow the minibuffer
    Given the window layout:
      """
      +---+---+
      | A | B |
      +---+---+
      """
    When I select window <window>
    And  I swallow-window down
    Then I should see message "swallow-window: can't swallow minibuffer"

    Examples:
      | window |
      | A      |
      | B      |

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

  Scenario Outline: No window there
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
    Then I should see message "swallow-window: no window there"

    Examples:
      | window | dir   |
      | C      | up    |
      | C      | right |
      | A      | up    |
      | A      | left  |
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

  Scenario Outline: 3 tall
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

  Scenario Outline: 3 wide
    Given the window layout:
      """
      +---+---+---+
      | A | B | C |
      +---+---+---+
      """
    When I select window <window>
    And  I swallow-window <dir1>
    Then window <swallowed1> should be deleted
    And  window <swallowed2> should be the same size
    When I swallow-window <dir2>
    Then window <swallowed2> should be deleted
    And  window <window> should be the only window in the frame

    Examples:
      | window | dir1  | swallowed1 | dir2  | swallowed2 |
      | A      | right | B          | right | C          |
      | B      | left  | A          | right | C          |
      | B      | right | C          | left  | A          |
      | C      | left  | B          | left  | A          |

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
    And  window <resized> should be shorter

    Examples:
      | window | dir   | resized | relation |
      | A      | right | C       | below    |
      | B      | right | C       | above    |

  Scenario Outline: Downsizing consecutive windows
    Given the window layout:
      """
      +---+---+---+---+
      | A |   |   | E |
      +---+ C | D +---+
      | B |   |   | F |
      +---+---+---+---+
      """
    When I select window <window>
    And  I swallow-window <dir>
    Then window <first> should be shorter
    And  window <second> should be the same size
    When I swallow-window <dir>
    Then window <second> should be shorter
    When I swallow-window <dir>
    Then window <last> should be deleted
    And  window <safe> should be the same size
    And  window <window> should be the full frame width

    Examples:
      | window | dir   | first | second | last | safe |
      | A      | right | C     | D      | E    | F    |
      | B      | right | C     | D      | F    | E    |
      | E      | left  | D     | C      | A    | B    |
      | F      | left  | D     | C      | B    | A    |

  Scenario Outline: Downsizing and swallowing
    Given the window layout:
      """
      +---+---+---+
      | A | B | C |
      +---+---+---+
      |     D     |
      +-----------+
      """
    When I select window <window>
    And  I swallow-window <dir1>
    Then window <window> should be the full frame height
    And  window D should be narrower
    When I swallow-window <dir2>
    Then window B should be deleted
    And  window D should be the same width as window <survivor>

    Examples:
      | window | dir1 | dir2  | survivor |
      | A      | down | right | C        |
      | C      | down | left  | A        |

  Scenario: We can't split a window in two
    Given the window layout:
      """
      +---+---+---+
      | A | B | C |
      +---+---+---+
      |     D     |
      +-----------+
      """
    When I select window B
    And  I swallow-window down
    Then window D should be deleted
    And  window A should be the full frame height
    And  window B should be the full frame height
    And  window C should be the full frame height
