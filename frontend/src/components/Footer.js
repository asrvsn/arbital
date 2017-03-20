import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'

import { SpeedDial, SpeedDialItem } from 'react-mui-speeddial';
import ContentCreate from 'material-ui/svg-icons/content/create'
import NavigationClose from 'material-ui/svg-icons/navigation/close'
import ContentAddBox from 'material-ui/svg-icons/content/add-box';
import AvPlaylistAdd from 'material-ui/svg-icons/av/playlist-add';

import CreateClaim from './dialogs/CreateClaim'
import CreateArgument from './dialogs/CreateArgument'
import { fireReload } from '../actions'

const styles = {
  footer: {
    position: 'fixed',
    bottom: 20,
    right: 15,
    zIndex: 1000
  }
}

class Footer extends Component {
  constructor(props) {
    super(props)
    this.state = {
      creatorOpen: null
    }
  }

  render() {
    const creator = this.getCreator()

    return (
      <div style={styles.footer}>
        <SpeedDial
          fabContentOpen={<ContentCreate />}
          fabContentClose={<NavigationClose />}
        >
          <SpeedDialItem
            label="New claim"
            fabContent={<ContentAddBox />}
            onTouchTap={e => this.openCreator('claim')}
          />

          <SpeedDialItem
            label="New argument"
            fabContent={<AvPlaylistAdd />}
            onTouchTap={e => this.openCreator('argument')}
          />

        </SpeedDial>

        {creator}

      </div>
    )
  }

  getCreator() {
    const { creatorOpen } = this.state

    switch(creatorOpen) {
      case 'claim':
        return (
          <CreateClaim
            open={true}
            onRequestClose={claim => this.closeCreatorAndReload(claim, 'claims')}
          />
        )
      case 'argument':
        return (
          <CreateArgument
            open={true}
            onRequestClose={arg => this.closeCreatorAndReload(arg, 'arguments')}
          />
        )
      default:
        return <noscript />
    }
  }

  openCreator(creator) {
    this.setState({creatorOpen: creator})
  }

  closeCreatorAndReload(result, dataSource) {
    this.setState({creatorOpen: null})
    if (result !== null) {
      this.props.dispatch(fireReload(dataSource))
    }
  }
}

export default connect(
  null,
  (dispatch) => ({ dispatch })
)(Footer)
