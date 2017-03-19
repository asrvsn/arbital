import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'

import FloatingActionButton from 'material-ui/FloatingActionButton';
import { SpeedDial, SpeedDialItem } from 'react-mui-speeddial';

import ContentCreate from 'material-ui/svg-icons/content/create'
import NavigationClose from 'material-ui/svg-icons/navigation/close'
import ContentAddBox from 'material-ui/svg-icons/content/add-box';
import AvPlaylistAdd from 'material-ui/svg-icons/av/playlist-add';

import CreateClaim from './CreateClaim'
import CreateArgument from './CreateArgument'

import { setClaimsDirty, setArgumentsDirty } from '../actions'

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
    const { dispatch } = this.props

    switch(creatorOpen) {
      case 'claim':
        return (
          <CreateClaim
            open={true}
            onRequestClose={claim => dispatch(setClaimsDirty(true))}
          />
        )
      case 'argument':
        return (
          <CreateArgument
            open={true}
            onRequestClose={arg => dispatch(setArgumentsDirty(true))}
          />
        )
      default:
        return <noscript />
    }
  }

  openCreator(creator) {
    this.setState({creatorOpen: creator})
  }

  closeCreator() {
    this.setState({creatorOpen: null})
  }
}

export default connect(
  null,
  dispatch => ({dispatch})
)(Footer)
