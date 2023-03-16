const schema = require('../../../../examples/kill-team-tracker.json');

describe('kill team tracker', () => {
  beforeEach(() => {
    // Not super graceful bug makes the tests a lot faster
    cy.visit('http://localhost:8000/src/Main.elm?track=ewogICJuYW1lIjogIktpbGwgVGVhbSBUcmFja2VyIiwKICAidHJhY2tlciI6IHsKICAgICJ0eXBlIjogImdyb3VwIiwKICAgICJpdGVtcyI6IFsKICAgICAgewogICAgICAgICJ0eXBlIjogImFjdGlvbiIsCiAgICAgICAgInRleHQiOiAiTmV4dCBUdXJuaW5nIFBvaW50IiwKICAgICAgICAiZWZmZWN0cyI6IFsKICAgICAgICAgIHsKICAgICAgICAgICAgInR5cGUiOiAiYWRqdXN0IiwKICAgICAgICAgICAgImFtb3VudCI6IDEsCiAgICAgICAgICAgICJ0YXJnZXRJZCI6ICJ0dXJuaW5nLXBvaW50IgogICAgICAgICAgfSwKICAgICAgICAgIHsKICAgICAgICAgICAgInR5cGUiOiAiYWRqdXN0IiwKICAgICAgICAgICAgImFtb3VudCI6IDEsCiAgICAgICAgICAgICJ0YXJnZXRJZCI6ICJjb21tYW5kLXBvaW50cyIsCiAgICAgICAgICAgICJzY29wZSI6ICJhbGwtcGxheWVycyIKICAgICAgICAgIH0KICAgICAgICBdCiAgICAgIH0sCiAgICAgIHsKICAgICAgICAidHlwZSI6ICJudW1iZXIiLAogICAgICAgICJ0ZXh0IjogIlR1cm5pbmcgUG9pbnQiLAogICAgICAgICJkZWZhdWx0IjogMSwKICAgICAgICAiZGlzYWJsZWQiOiB0cnVlLAogICAgICAgICJpZCI6ICJ0dXJuaW5nLXBvaW50IgogICAgICB9LAogICAgICB7CiAgICAgICAgInR5cGUiOiAicGxheWVyLWdyb3VwIiwKICAgICAgICAibWluUGxheWVycyI6IDIsCiAgICAgICAgIm1heFBsYXllcnMiOiAyLAogICAgICAgICJpdGVtcyI6IFsKICAgICAgICAgIHsKICAgICAgICAgICAgInR5cGUiOiAiYWN0aW9uIiwKICAgICAgICAgICAgInRleHQiOiAiSGFzIEluaXRpYXRpdmUiLAogICAgICAgICAgICAiZWZmZWN0cyI6IFsKICAgICAgICAgICAgICB7CiAgICAgICAgICAgICAgICAidHlwZSI6ICJzZXQtY3VycmVudC1wbGF5ZXIiLAogICAgICAgICAgICAgICAgInRhcmdldCI6ICJ0aGlzLXBsYXllciIKICAgICAgICAgICAgICB9CiAgICAgICAgICAgIF0KICAgICAgICAgIH0sCiAgICAgICAgICB7CiAgICAgICAgICAgICJ0eXBlIjogIm51bWJlciIsCiAgICAgICAgICAgICJ0ZXh0IjogIkNvbW1hbmQgUG9pbnRzIiwKICAgICAgICAgICAgImRlZmF1bHQiOiA0LAogICAgICAgICAgICAibWluIjogMCwKICAgICAgICAgICAgIm1heCI6IDk5LAogICAgICAgICAgICAiaWQiOiAiY29tbWFuZC1wb2ludHMiCiAgICAgICAgICB9LAogICAgICAgICAgewogICAgICAgICAgICAidHlwZSI6ICJudW1iZXIiLAogICAgICAgICAgICAidGV4dCI6ICJQcmltYXJ5IFZQIiwKICAgICAgICAgICAgImRlZmF1bHQiOiAwLAogICAgICAgICAgICAibWluIjogMCwKICAgICAgICAgICAgIm1heCI6IDk5LAogICAgICAgICAgICAiaWQiOiAicHJpbWFyeS12cCIKICAgICAgICAgIH0sCiAgICAgICAgICB7CiAgICAgICAgICAgICJ0eXBlIjogIm51bWJlciIsCiAgICAgICAgICAgICJ0ZXh0IjogIlNlY29uZGFyeSBWUCIsCiAgICAgICAgICAgICJkZWZhdWx0IjogMCwKICAgICAgICAgICAgIm1pbiI6IDAsCiAgICAgICAgICAgICJtYXgiOiA5OSwKICAgICAgICAgICAgImlkIjogInNlY29uZGFyeS12cCIKICAgICAgICAgIH0sCiAgICAgICAgICB7CiAgICAgICAgICAgICJ0eXBlIjogImNhbGN1bGF0ZWQiLAogICAgICAgICAgICAidGV4dCI6ICJUb3RhbCBWUCIsCiAgICAgICAgICAgICJlcXVhbHMiOiB7CiAgICAgICAgICAgICAgInR5cGUiOiAiYWRkIiwKICAgICAgICAgICAgICAib3BzIjogWwogICAgICAgICAgICAgICAgewogICAgICAgICAgICAgICAgICAidHlwZSI6ICJyZWYiLAogICAgICAgICAgICAgICAgICAidGFyZ2V0SWQiOiAicHJpbWFyeS12cCIsCiAgICAgICAgICAgICAgICAgICJzY29wZSI6ICJ0aGlzLXBsYXllciIKICAgICAgICAgICAgICAgIH0sCiAgICAgICAgICAgICAgICB7CiAgICAgICAgICAgICAgICAgICJ0eXBlIjogInJlZiIsCiAgICAgICAgICAgICAgICAgICJ0YXJnZXRJZCI6ICJzZWNvbmRhcnktdnAiLAogICAgICAgICAgICAgICAgICAic2NvcGUiOiAidGhpcy1wbGF5ZXIiCiAgICAgICAgICAgICAgICB9CiAgICAgICAgICAgICAgXQogICAgICAgICAgICB9CiAgICAgICAgICB9LAogICAgICAgICAgewogICAgICAgICAgICAidHlwZSI6ICJncm91cCIsCiAgICAgICAgICAgICJ0ZXh0IjogIk9wZXJhdGl2ZXMiLAogICAgICAgICAgICAiY29sbGFwc2VkIjogdHJ1ZSwKICAgICAgICAgICAgImlkIjogIm9wZXJhdGl2ZXMiLAogICAgICAgICAgICAiaXRlbXMiOiBbCiAgICAgICAgICAgICAgewogICAgICAgICAgICAgICAgInR5cGUiOiAiaXRlbS1saXN0IiwKICAgICAgICAgICAgICAgICJ0ZXh0IjogIk9wZXJhdGl2ZSIsCiAgICAgICAgICAgICAgICAiaWQiOiAib3BlcmF0aXZlcyIsCiAgICAgICAgICAgICAgICAiaXRlbXMiOiBbCiAgICAgICAgICAgICAgICAgIHsKICAgICAgICAgICAgICAgICAgICAidHlwZSI6ICJ0ZXh0IiwKICAgICAgICAgICAgICAgICAgICAidGV4dCI6ICJOYW1lIiwKICAgICAgICAgICAgICAgICAgICAiaWQiOiAib3BlcmF0aXZlLW5hbWUiCiAgICAgICAgICAgICAgICAgIH0sCiAgICAgICAgICAgICAgICAgIHsKICAgICAgICAgICAgICAgICAgICAidHlwZSI6ICJudW1iZXIiLAogICAgICAgICAgICAgICAgICAgICJ0ZXh0IjogIldvdW5kcyIsCiAgICAgICAgICAgICAgICAgICAgImRlZmF1bHQiOiAwLAogICAgICAgICAgICAgICAgICAgICJtaW4iOiAwLAogICAgICAgICAgICAgICAgICAgICJpZCI6ICJvcGVyYXRpdmUtd291bmRzIgogICAgICAgICAgICAgICAgICB9CiAgICAgICAgICAgICAgICBdCiAgICAgICAgICAgICAgfQogICAgICAgICAgICBdCiAgICAgICAgICB9CiAgICAgICAgXQogICAgICB9CiAgICBdCiAgfQp9Cg%3D%3D')
  });

  describe('when tracking', () => {
    describe('when a player adds an operative', () => {
      beforeEach(() => {
        cy.get('#player-1-operatives-add-item').click();
      });

      describe('the operative', () => {
        it('exists', () => {
          cy.get('#player-1-item-0-Name-text').should('exist')
        });
      });

      describe('when they undo the addition', () => {
        beforeEach(() => {
          cy.get('#undo-last-history-entry').click();
        });

        describe('the operative', () => {
           it('no longer exists', () => {
              cy.get('#player-1-item-0-Name-text').should('not.exist')
           });
        });
      });

      describe('and they add wounds to the operative', () => {
        beforeEach(() => {
          cy.get('#player-1-item-0-operative-wounds-number input').type('{selectAll}12');
        });

       describe('when they remove that operative', () => {
          beforeEach(() => {
            cy.get('#player-1-item-0-operatives-remove-item').click();
          });

          describe('the operative', () => {
            it('no longer exists', () => {
               cy.get('#player-1-item-0-Name-text').should('not.exist')
            });
          });

          describe('when they undo the removal', () => {
            beforeEach(() => {
               cy.get('#undo-last-history-entry').click();
            });

            describe('the operative', () => {
              it('is back with the correct wounds', () => {
                cy.get('#player-1-item-0-Name-text').should('exist')
                cy.get('#player-1-item-0-operative-wounds-number input').should('have.value', 12)
              });
            });
          });
       });

      });
    });

    describe('at the start of the game', () => {
      describe('the turning point', () => {
        it("is 1", () => {
          cy.get('#turning-point-number .number-value').should('contain', '1')
        });
      });

      describe('player 1', () => {
        it("has initiative", () => {
           cy.get('.player-0.current-player').should('exist')
           cy.get('.player-1.player').should('exist')
           cy.get('.player-1.current-player').should('not.exist')
        });
      });

      describe('each player', () => {
        it("has 4 command points", () => {
          cy.get('#player-0-command-points-number input').should('have.value', 4)
          cy.get('#player-1-command-points-number input').should('have.value', 4)
        });
      });
    });

    describe('when each player has spent some command points and earned some primary and secondary victory points', () => {
      beforeEach(() => {
        cy.get('#player-0-command-points-number input').type('{selectAll}2')
        cy.get('#player-0-primary-vp-number input').type('{selectAll}1')
        cy.get('#player-0-secondary-vp-number input').type('{selectAll}2')

        cy.get('#player-1-command-points-number input').type('{selectAll}1')
        cy.get('#player-1-primary-vp-number input').type('{selectAll}1')
        cy.get('#player-1-secondary-vp-number input').type('{selectAll}1')
      });

      describe('victory point total', () => {
        it('is a sum of primary and secondary VP', () => {
          cy.get('#player-0-Total-VP-calculated .calculation-result').should('contain', '3')
          cy.get('#player-1-Total-VP-calculated .calculation-result').should('contain', '2')
        });
      });

      describe('when undo-ing the last of the adjustments', () => {
        beforeEach(() => {
          cy.get('#undo-last-history-entry').click();
        });

        describe('the field whose adjustment was undone', () => {
          it('reflects the undone adjustment', () => {
            cy.get('#player-1-secondary-vp-number input').should('have.value', 0)
          });
        });

        describe('victory point total', () => {
          it('reflects the undone adjustment', () => {
            cy.get('#player-0-Total-VP-calculated .calculation-result').should('contain', '3')
            cy.get('#player-1-Total-VP-calculated .calculation-result').should('contain', '1')
          });
        });
      });

      describe('when switching initiative to player 2', () => {
        describe('when switching initiative to player 2', () => {
          beforeEach(() => {
            cy.get('#player-1-Has-Initiative-action').click();
          });

          it("switches", () => {
            cy.get('.player-1.current-player').should('exist')
            cy.get('.player-0.player').should('exist')
            cy.get('.player-0.current-player').should('not.exist')
          });

          describe('when undo-ing', () => {
            beforeEach(() => {
              cy.get('#undo-last-history-entry').click();
            });

            it("switches back", () => {
              cy.get('.player-0.current-player').should('exist')
              cy.get('.player-1.player').should('exist')
              cy.get('.player-1.current-player').should('not.exist')
            });
          });
        });
      });

      describe('when moving to turning point 2', () => {
        beforeEach(() => {
          cy.get('#Next-Turning-Point-action').click();
        });

        describe('the turning point', () => {
          it("is 2", () => {
            cy.get('#turning-point-number .number-value').should('contain', '2')
          });
        });

        it("increments both players command points", () => {
          cy.get('#player-0-command-points-number input').should('have.value', 3)
          cy.get('#player-1-command-points-number input').should('have.value', 2)
        });

        describe('when undo-ing', () => {
          beforeEach(() => {
            cy.get('#undo-last-history-entry').click();
          });

          describe('the turning point', () => {
            it("is 1 again", () => {
              cy.get('#turning-point-number .number-value').should('contain', '1')
            });
          });

          it("decrements both players command points", () => {
            cy.get('#player-0-command-points-number input').should('have.value', 2)
            cy.get('#player-1-command-points-number input').should('have.value', 1)
          });
        });
      });
    });
  });
});
