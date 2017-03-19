import React, { Component, PropTypes } from 'react'

import FloatingActionButton from 'material-ui/FloatingActionButton';
import ContentCreate from 'material-ui/svg-icons/content/create';

import CreateClaim from './CreateClaim'

class Footer extends Component {
  constructor(props) {
    super(props)
    this.state = {
      creatorOpen: false
    }
  }

  render() {
    const { creatorOpen } = this.state

    return (
      <div>
        <FloatingActionButton
          onTouchTap={e => this.openCreator()}
          >
          <ContentCreate />
        </FloatingActionButton>
        { creatorOpen &&
          <CreateClaim
            onRequestClose={() => this.closeCreator()}
            />
        }
      </div>
    )
  }

  openCreator() {
    this.setState({creatorOpen: true})
  }

  closeCreator() {
    this.setState({creatorOpen: false})
  }
}

export default Footer
