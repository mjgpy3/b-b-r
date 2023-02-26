const schema = require('../../../../examples/kill-team-tracker.json');

describe('kill team tracker', () => {
  beforeEach(() => {
    cy.visit('http://localhost:8000/src/Main.elm')
  });

  describe('when tracking', () => {
    beforeEach(() => {
      cy.get('textarea').type(JSON.stringify(schema), { parseSpecialCharSequences: false, delay: 0 });
      cy.get('.create-tracker-button').click()
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
      });
    });
  });
});
