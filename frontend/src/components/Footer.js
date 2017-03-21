import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'

import ContentCreate from 'material-ui/svg-icons/content/create'
import NavigationClose from 'material-ui/svg-icons/navigation/close'
import ContentAddBox from 'material-ui/svg-icons/content/add-box';
import AvPlaylistAdd from 'material-ui/svg-icons/av/playlist-add';
import { SpeedDial, BubbleList, BubbleListItem } from 'react-speed-dial';
import Avatar from 'material-ui/Avatar';

import CreateClaim from './dialogs/CreateClaim'
import CreateArgument from './dialogs/CreateArgument'
import { openDialog } from '../actions'

const styles = {
  footer: {
    position: 'fixed',
    bottom: 20,
    right: 15,
    zIndex: 1000
  }
}

class Footer extends Component {

  render() {
    const { dispatch } = this.props

    const createClaimIcon = (
      <Avatar icon={<ContentAddBox />} />
    )
    const createArgumentIcon = (
      <Avatar icon={<AvPlaylistAdd />} />
    )

    const openArgumentDialog = () => {
      this.closeSpeedDial()
      dispatch(openDialog({
        dialogType: 'CREATE_ARGUMENT',
        payload: {}
      }))
    }

    const openClaimDialog = () => {
      this.closeSpeedDial()
      dispatch(openDialog({
        dialogType: 'CREATE_CLAIM',
        payload: {}
      }))
    }

    return (
      <div style={styles.footer}>

        <SpeedDial
          ref={elem => this.speedDial = elem}
        >
          <BubbleList>

            <BubbleListItem
              primaryText="Create claim"
              rightAvatar={createClaimIcon}
              onTouchTap={e => openClaimDialog()}
            />

            <BubbleListItem
              primaryText="Create argument"
              rightAvatar={createArgumentIcon}
              onTouchTap={e => openArgumentDialog()}
            />

          </BubbleList>
        </SpeedDial>

      </div>
    )
  }

  closeSpeedDial() {
    this.speedDial.setState({isOpen: false})
  }
}

export default connect(
  null,
  (dispatch) => ({ dispatch })
)(Footer)
