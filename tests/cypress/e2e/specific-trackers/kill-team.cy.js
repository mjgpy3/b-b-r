const schema = require('../../../../examples/kill-team-tracker.json');

describe('kill team tracker', () => {
  beforeEach(() => {
    const trackerEncoding = encodeURIComponent(btoa(JSON.stringify(schema)));
    cy.visit(`http://localhost:8000/src/Main.elm?track=${trackerEncoding}`)
  });

  describe('when tracking', () => {
    describe('when a player adds a few operatives', () => {
      beforeEach(() => {
        [0, 1, 2, 3, 4, 5, 6].forEach(() => {
          cy.get('#player-1-operatives-add-item').click();
        });
      });

      describe('those operatives', () => {
        it('exist', () => {
          [0, 1, 2, 3, 4, 5, 6].forEach(i => {
            cy.get(`#player-1-item-${i}-Name-text`).should('exist')
          });
        });
      });

      describe('when they selectively remove some of those operatives', () => {
        beforeEach(() => {
          [2, 4, 5].forEach(i => {
            cy.get(`#player-1-item-${i}-operatives-remove-item`).click();
          });
        });

       describe('the remaining operatives', () => {
          it('exist', () => {
            [0, 1, 3, 6].forEach(i => {
               cy.get(`#player-1-item-${i}-Name-text`).should('exist')
            });
          });
       });

       describe('the removed operatives', () => {
          it('do not exist', () => {
              [2, 4, 5].forEach(i => {
               cy.get(`#player-1-item-${i}-Name-text`).should('not.exist')
            });
          });
       });
      })
    });

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
