import React, { Component, PropTypes } from 'react'

import FloatingActionButton from 'material-ui/FloatingActionButton';
import { SpeedDial, SpeedDialItem } from 'react-mui-speeddial';

import ContentAdd from 'material-ui/svg-icons/content/add'
import NavigationClose from 'material-ui/svg-icons/navigation/close'
import ContentAddBox from 'material-ui/svg-icons/content/add-box';
import AvPlaylistAdd from 'material-ui/svg-icons/av/playlist-add';

import CreateClaim from './CreateClaim'
import CreateArgument from './CreateArgument'

class Footer extends Component {
  constructor(props) {
    super(props)
    this.state = {
      creatorOpen: null
    }
  }

  render() {
    const { creatorOpen } = this.state

    return (
      <div>
        <SpeedDial
          fabContentOpen={<ContentAdd />}
          fabContentClose={<NavigationClose />}
        >
          <SpeedDialItem
            label="New claim"
            fabContent={<ContentAddBox />}
            onTouchTap={e => this.openCreator('argument')}
          />

          <SpeedDialItem
            label="New argument"
            fabContent={<AvPlaylistAdd />}
            onTouchTap={e => this.openCreator('claim')}
          />

        </SpeedDial>

        <CreateClaim open={creatorOpen === 'claim'} />
        <CreateArgument open={creatorOpen === 'argument'} />

      </div>
    )
  }

  openCreator(creator) {
    this.setState({creatorOpen: creator})
  }

  closeCreator() {
    this.setState({creatorOpen: null})
  }
}

export default Footer
