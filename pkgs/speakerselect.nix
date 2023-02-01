{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellApplication {
  name = "speakerselect";

  runtimeInputs = with pkgs; [
    pulseaudio
  ];

  text = ''
    sink_ids="$(pactl list sinks | grep 'Sink ' | cut -d '#' -f 2)"
    sink_descs="$(pactl list sinks | grep 'Description:' | cut -d ' ' -f 3-)"
    cur_sources="$(pactl list sink-inputs | grep 'Sink Input' | cut -d '#' -f 2)"

    declare -A desc_ids

    # Populate the description and sink ids
    IFS=$'\n'
    #unset result
    for (( i=0; i<''${#sink_ids[*]}; ++i)); do 
      sd=''${sink_descs[$i]}
      si=''${sink_ids[$i]}
      desc_ids[$sd]=$si
    done

    chosen=$(printf "%s" "$sink_descs" | dmenu -i -p "Select output:")

    # only move things if the command completed successfully
    if [[ -z "$chosen" ]]; then
      exit 0
    fi

    output_id="''${desc_ids[$chosen]}"

    # move everything over to the new output
    pacmd set-default-sink "$output_id"
    for source in $cur_sources; do
      pacmd move-sink-input "$source" "$output_id"
    done
  '';
}

